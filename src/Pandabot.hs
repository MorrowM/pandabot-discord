module Pandabot
  ( runBotWith
  , main
  ) where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands                      hiding (path)
import           Calamity.Commands.Context
import           Calamity.Gateway
import           Calamity.Metrics.Noop
import           Calamity.Types.Model.Presence.Activity as Activity
import           Control.Monad
import qualified Data.Aeson                             as Aeson
import           Data.Flags
import           Data.List
import qualified Data.Map                               as Map
import qualified Data.Sequence                          as Seq
import qualified Data.Yaml                              as Yaml
import qualified Database.Persist.Sql                   as DB
import qualified Df1
import qualified Di
import qualified Di.Core
import qualified DiPolysemy                             as DiP
import           Optics
import           Options.Generic
import qualified Polysemy                               as P
import qualified Polysemy.AtomicState                   as P
import qualified Polysemy.Reader                        as P
import qualified Polysemy.Time                          as P
import           System.Directory
import           System.Exit

import           Pandabot.Bot.Commands
import           Pandabot.Bot.Config
import           Pandabot.Bot.Database
import           Pandabot.Bot.Handlers
import           Pandabot.Bot.Schema
import           Pandabot.Bot.Util
import           Pandabot.Modtools
import           Pandabot.PlayerDB.Whitelist
import           Pandabot.Points

-- | Run the bot with a given configuration.
runBotWith :: Config -> IO ()
runBotWith cfg = Di.new $ \di ->
  void
  . P.runFinal
  . P.embedToFinal @IO
  . DiP.runDiToIO di
--  . DiP.local filterShard
  . runCacheInMemory
  . runMetricsNoop
  . runPersistWith (cfg ^. #connectionString)
  . useConstantPrefix (cfg ^. #commandPrefix)
  . useFullContext
  . runReqInIO
  . P.runReader cfg
  . P.interpretTimeGhc
  . P.atomicStateToIO (MessagePointMessages Map.empty)
  . P.atomicStateToIO Unlocked
  . runBotIO'
    (BotToken (cfg ^. #botToken))
    (defaultIntents .+. intentGuildMembers .+. intentGuildPresences)
    (Just (StatusUpdateData Nothing [botActivity] Online False))
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

_filterShard :: Di.Core.Di level Di.Path msg -> Di.Core.Di level Di.Path msg
_filterShard = Di.Core.filter $ \_ path _ ->
  path /= shardPath
  where
    shardPath = Seq.fromList [Df1.Push "calamity", Df1.Push "calamity-shard", Df1.Attr "shard-id" "0"]

botActivity :: Activity
botActivity = Activity.activity "Minecraft" Game
