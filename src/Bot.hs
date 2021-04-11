module Bot where

import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Metrics.Noop
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import qualified Database.Persist.Sql    as DB
import qualified Di
import qualified DiPolysemy              as DiP
import qualified Polysemy                as P

import           Commands
import           Database
import           Handlers
import           Schema
import           Types
import           Util

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
  $ runBotIO (BotToken (config ^. #botToken . lazy)) defaultIntents $ handleFailByLogging $ do
    db $ DB.runMigration migrateAll
    registerBotCommands
    registerEventHandlers

main :: IO ()
main = do
  Right config <- eitherDecodeFileStrict "bot.json"
  runBotWith config
