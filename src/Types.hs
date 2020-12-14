{-# LANGUAGE OverloadedStrings #-}
module Types 
  ( Handler
  , runHandler
  , runDB
  , execDB
  , run
  , exec
  , catchErr
  , raiseErr
  , assertTrue
  , assertJust
  , getDis
  , getConfig
  , NameError (..)
  ) where

import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.Except
    ( ExceptT(..), runExceptT, throwE )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )
import Data.Text (Text)
import Data.Time ( defaultTimeLocale, getCurrentTime, formatTime )
import Discord ( FromJSON, DiscordHandle, restCall )
import Discord.Internal.Rest.Prelude ( Request )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Config ( App (..), Config (..) )
import Database ( DatabaseAction, db )

type Handler a = ExceptT Text (ReaderT App IO) a

runHandler :: App -> Handler () -> IO ()
runHandler dis h = do
  eith <- flip runReaderT dis $ runExceptT h
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
    Left err -> throwE . T.pack . show $ err
    Right a -> pure a

exec :: (FromJSON a, Request (r a)) => r a -> Handler ()
exec = void . run

catchErr :: Handler () -> Handler ()
catchErr h = do
  dis <- lift ask
  eitherVal <- liftIO $ runReaderT (runExceptT h) dis
  case eitherVal of
    Left txt -> liftIO $ TIO.putStr txt
    Right val -> return val

raiseErr :: Text -> Handler a
raiseErr txt = ExceptT $ return $ Left txt

assertTrue :: Bool -> Handler ()
assertTrue True = return ()
assertTrue False = raiseErr ""

assertJust :: Maybe a -> Handler a
assertJust Nothing = raiseErr ""
assertJust (Just x) = pure x

getApp :: Handler App
getApp = lift ask

getDis :: Handler DiscordHandle
getDis = appDis <$> getApp

getConfig :: Handler Config
getConfig = appConfig <$> getApp

data NameError a = NameNotFound | NameAmbiguous [a]