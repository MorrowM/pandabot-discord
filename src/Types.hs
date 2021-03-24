
module Types 
  ( Handler
  , runHandler
  , runDB
  , execDB
  , run
  , exec
  , catchErr
  , assertTrue
  , assertJust
  , getDis
  , getConfig
  , NameError (..)
  ) where

import Control.Monad ( void )
import Control.Monad.Trans (MonadIO (liftIO), MonadTrans (lift))
import Control.Monad.Except
    ( ExceptT(..), runExceptT, throwError, MonadError )
import Control.Monad.Reader ( ReaderT(runReaderT), ask, MonadReader )
import Data.Text (Text)
import Data.Time ( defaultTimeLocale, getCurrentTime, formatTime )
import Discord ( FromJSON, DiscordHandle, restCall )
import Discord.Internal.Rest.Prelude ( Request )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Config ( App (..), Config (..) )
import Database ( DatabaseAction, db )

newtype Handler a = Handler
  { getHandler :: ExceptT Text (ReaderT App IO) a
  }
  deriving (Functor, Applicative, Monad, MonadError Text, MonadReader App, MonadIO)

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

runDB :: DatabaseAction a -> Handler a
runDB = liftIO . db

execDB :: DatabaseAction a -> Handler ()
execDB = void . runDB

run :: (FromJSON a, Request (r a)) => r a -> Handler a
run r = do
  dis <- getDis
  res <- liftIO $ restCall dis r
  case res of
    Left err -> throwError . T.pack . show $ err
    Right a -> pure a

exec :: (FromJSON a, Request (r a)) => r a -> Handler ()
exec = void . run

catchErr :: Handler () -> Handler ()
catchErr h = do
  dis <- ask
  eitherVal <- liftIO $ runReaderT (runExceptT $ getHandler h) dis
  case eitherVal of
    Left txt -> liftIO $ TIO.putStr txt
    Right val -> pure val

assertTrue :: Bool -> Handler ()
assertTrue True = pure ()
assertTrue False = throwError ""

assertJust :: Maybe a -> Handler a
assertJust Nothing = throwError ""
assertJust (Just x) = pure x

getApp :: Handler App
getApp = ask

getDis :: Handler DiscordHandle
getDis = appDis <$> getApp

getConfig :: Handler Config
getConfig = appConfig <$> getApp

data NameError a = NameNotFound | NameAmbiguous [a]