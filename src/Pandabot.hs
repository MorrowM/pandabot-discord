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
import           Data.Aeson
import           Data.Flags
import qualified Data.Map                as Map
import qualified Database.Persist.Sql    as DB
import qualified Di
import qualified DiPolysemy              as DiP
import qualified Polysemy                as P
import qualified Polysemy.AtomicState    as P
import qualified Polysemy.Reader         as P

import           Pandabot.Commands
import           Pandabot.Database
import           Pandabot.Handlers
import           Pandabot.Schema
import           Pandabot.Types
import           Pandabot.Util

-- | Run the bot with a given configuration.
runBotWith :: Config -> IO ()
runBotWith config = Di.new $ \di ->
  void
  . P.runFinal
  . P.embedToFinal @IO
  . DiP.runDiToIO di
  . runCacheInMemory
  . runMetricsNoop
  . runPersistWith (config ^. #connectionString)
  . useConstantPrefix (config ^. #commandPrefix . lazy)
  . P.runReader config
  . P.atomicStateToIO (PointMessages Map.empty)
  . runBotIO (BotToken (config ^. #botToken . lazy)) (defaultIntents .+. intentGuildMembers .+. intentGuildPresences)
  . handleFailByLogging $ do
    db $ DB.runMigration migrateAll
    registerBotCommands
    registerEventHandlers

-- | Run the bot in the `IO` monad, reading the configuration
-- from a `bot.json` file.
main :: IO ()
main = do
  mconfig <- eitherDecodeFileStrict "bot.json"
  case mconfig of
    Left err     -> print err
    Right config -> runBotWith config
