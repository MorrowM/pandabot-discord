
module Types
  ( Handler
  , runHandler
  , runDB
  , runDB_
  , run
  , run_
  , catchErr
  , assertTrue
  , assertJust
  , getDis
  , getConfig
  , NameError (..)
  ) where

import           Control.Monad                 (void)
import           Control.Monad.Except          (ExceptT (..), MonadError,
                                                runExceptT, throwError)
import           Control.Monad.Reader          (MonadReader,
                                                ReaderT (runReaderT), ask, asks)
import           Control.Monad.Trans           (MonadIO (liftIO),
                                                MonadTrans (lift))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import           Data.Time                     (defaultTimeLocale, formatTime,
                                                getCurrentTime)
import           Discord                       (DiscordHandle, FromJSON,
                                                restCall)
import           Discord.Internal.Rest.Prelude (Request)

import           Config                        (App (..), Config (..))
import           Database                      (DatabaseAction, db)

-- | The main monad stack for the application.
newtype Handler a = Handler
  { getHandler :: ExceptT Text (ReaderT App IO) a
  }
  deriving (Functor, Applicative, Monad, MonadError Text, MonadReader App, MonadIO)

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
      TIO.putStrLn $ fmt <> s

-- | Run a database action in the @Handler@ monad.
runDB :: DatabaseAction a -> Handler a
runDB = liftIO . db

-- | Like @runDB@, discarding the result.
runDB_ :: DatabaseAction a -> Handler ()
runDB_ = void . runDB

-- | Run a Discord API request in the @Handler@ monad.
run :: (FromJSON a, Request (r a)) => r a -> Handler a
run r = do
  dis <- getDis
  res <- liftIO $ restCall dis r
  case res of
    Left err -> throwError . T.pack . show $ err
    Right a  -> pure a

-- | Like @run@, discarding the result.
run_ :: (FromJSON a, Request (r a)) => r a -> Handler ()
run_ = void . run

-- | Catch any errors, printing them to the console.
catchErr :: Handler () -> Handler ()
catchErr h = do
  dis <- ask
  eitherVal <- liftIO $ runReaderT (runExceptT $ getHandler h) dis
  case eitherVal of
    Left txt  -> liftIO $ TIO.putStr txt
    Right val -> pure val

-- | Throw an error given a @Bool@.
assertTrue :: Bool -> Handler ()
assertTrue True  = pure ()
assertTrue False = throwError ""

-- | Extract a result from a @Maybe@, throwing an error if the input is @Nothing@.
assertJust :: Maybe a -> Handler a
assertJust Nothing  = throwError ""
assertJust (Just x) = pure x

-- | Retrieve the @DiscordHandle@ from the application context.
getDis :: Handler DiscordHandle
getDis = asks appDis

-- | Retrieve the @Config@ from the application context.
getConfig :: Handler Config
getConfig = asks appConfig

-- | Represents a lookup error, where the lookup expects exactly one result
-- but none or multiple are found.
data NameError a = NameNotFound | NameAmbiguous [a]
