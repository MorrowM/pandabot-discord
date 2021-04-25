module Pandabot
  ( runBotWith
  , main
  ) where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Metrics.Noop
import           Control.Lens
import           Control.Monad
import qualified Data.Aeson              as Aeson
import           Data.Flags
import           Data.List
import qualified Data.Map                as Map
import qualified Data.Yaml               as Yaml
import qualified Database.Persist.Sql    as DB
import qualified Di
import qualified DiPolysemy              as DiP
import           Options.Generic
import qualified Polysemy                as P
import qualified Polysemy.AtomicState    as P
import qualified Polysemy.Reader         as P
import qualified Polysemy.Time           as P
import           System.Directory
import           System.Exit

import           Pandabot.Commands
import           Pandabot.Database
import           Pandabot.Handlers
import           Pandabot.Schema
import           Pandabot.Types
import           Pandabot.Util

-- | Run the bot with a given configuration.
runBotWith :: Config -> IO ()
runBotWith cfg = Di.new $ \di ->
  void
  . P.runFinal
  . P.embedToFinal @IO
  . DiP.runDiToIO di
  . runCacheInMemory
  . runMetricsNoop
  . runPersistWith (cfg ^. #connectionString)
  . useConstantPrefix (cfg ^. #commandPrefix . lazy)
  . P.runReader cfg
  . P.interpretTimeGhc
  . P.atomicStateToIO (MessagePointMessages Map.empty)
  . runBotIO (BotToken (cfg ^. #botToken . lazy)) (defaultIntents .+. intentGuildMembers .+. intentGuildPresences)
  . handleFailByLogging $ do
    db $ DB.runMigration migrateAll
    registerBotCommands
    registerEventHandlers

-- | Run the bot in the `IO` monad, reading the configuration
-- from a `bot.json` file.
main :: IO ()
main = do
  opts <- unwrapRecord @_ @CLIOptions "Pandabot - a bot for pandas"
  path <- case opts ^. #config of
    Just path -> pure path
    Nothing -> do
      ifM (doesFileExist "bot.json") ("bot.json" <$ putStrLn "using bot.json...") $
        ifM (doesFileExist "bot.yaml") ("bot.yaml" <$ putStrLn "using bot.yaml...") $
          die "error: cannot find configuration file"
  cfg <-
    if "yaml" `isSuffixOf` path
    then Yaml.decodeFileThrow path
    else if "json" `isSuffixOf` path
    then Aeson.eitherDecodeFileStrict path >>= either die pure
    else die "error: unrecoognized file extension (must be either json or yaml)"
  runBotWith cfg
  where
    ifM mb x y = do
      b <- mb
      if b then x else y
