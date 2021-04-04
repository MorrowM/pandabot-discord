{-# LANGUAGE UndecidableInstances #-}
module Types
  ( Handler
  , runHandler
  , runDB
  , runDB_
  , catchErr
  , assertTrue
  , assertJust
  , getConfig
  , NameError (..)
  , MonadDiscord (..)
  , parseConfigFile
  , fetchCache
  , App
  , Config
  , Cache
  ) where

import           Control.Concurrent
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Bifunctor
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Data.Time
import           Database.Persist
import           Discord                       (DiscordHandle, restCall)
import           Discord.Internal.Rest.Prelude (Request)
import           Discord.Types
import           GHC.Generics

import           Database
import           Schema

-- | The main monad stack for the application.
newtype Handler a = Handler
  { getHandler :: ExceptT Text (ReaderT App IO) a
  }
  deriving (Functor, Applicative, Monad, MonadError Text, MonadReader App, MonadIO)

class (Monad m, MonadIO m, MonadError Text m) => MonadDiscord m where
  -- | Retrieve the Discord API handle.
  getDis :: m DiscordHandle

  -- | Run a Discord API request.
  run :: (FromJSON a, Request (r a)) => r a -> m a
  run r = do
    dis <- getDis
    res <- liftIO $ restCall dis r
    case res of
      Left err -> throwError . T.pack . show $ err
      Right a  -> pure a

  -- | Like @run@, discarding the result.
  run_ :: (FromJSON a, Request (r a)) => r a -> m ()
  run_ = void . run

  {-# MINIMAL getDis #-}

instance
  ( Monad (t m)
  , MonadIO (t m)
  , MonadError Text (t m)
  , MonadTrans t
  , MonadDiscord m
  ) => MonadDiscord (t m) where
  getDis = lift getDis

instance MonadDiscord Handler where
  getDis = view #disHandle

-- | Escape from the @Handler@ monad into the @IO@ monad.
runHandler :: App -> Handler () -> IO ()
runHandler dis h = do
  eith <- flip runReaderT dis $ runExceptT $ getHandler h
  case eith of
    Left err -> logS err
    Right () -> pure ()
  where
    logS s = do
      t <- getCurrentTime
      let fmt = T.pack $ formatTime defaultTimeLocale "[%F %T] " t
      T.putStrLn $ fmt <> s

-- | Run a database action in the @Handler@ monad.
runDB :: MonadIO m => DatabaseAction a -> m a
runDB = liftIO . db

-- | Like @runDB@, discarding the result.
runDB_ :: MonadIO m => DatabaseAction a -> m ()
runDB_ = void . runDB

-- | Catch any errors, printing them to the console.
catchErr :: Handler () -> Handler ()
catchErr h = do
  app <- view id
  eitherVal <- liftIO $ runReaderT (runExceptT $ getHandler h) app
  case eitherVal of
    Left txt  -> liftIO $ T.putStr txt
    Right val -> pure val

-- | Throw an error given a @Bool@.
assertTrue :: Bool -> Handler ()
assertTrue True  = pure ()
assertTrue False = throwError ""

-- | Extract a result from a @Maybe@, throwing an error if the input is @Nothing@.
assertJust :: Maybe a -> Handler a
assertJust Nothing  = throwError ""
assertJust (Just x) = pure x

-- | Retrieve the @Config@ from the application context.
getConfig :: MonadReader App m => m Config
getConfig = view #config

-- | Represents a lookup error, where the lookup expects exactly one result
-- but none or multiple are found.
data NameError a = NameNotFound | NameAmbiguous [a]

-- | The application environment.
data App = App
  { disHandle :: DiscordHandle
  , config    :: Config
  , cache     :: MVar Cache
  } deriving Generic

data Cache = Cache
  { buttons            :: [Button]
  , pointAwardMessages :: [MessageId ]
  } deriving Generic

fetchCache :: MonadIO m => m Cache
fetchCache = do
  btns <- runDB $ fmap (map entityVal) $ runDB $ selectList [] []
  pure $ Cache { buttons = btns, pointAwardMessages = [] }

-- | The application configuration
data Config = Config
  { botToken           :: Text
  , welcomeRole        :: RoleId
  , pointAssignEmoji   :: Text
  , reactPositiveEmoji :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config

parseConfigFile :: FilePath -> ExceptT Text IO Config
parseConfigFile path = ExceptT $ first T.pack <$> eitherDecodeFileStrict path
