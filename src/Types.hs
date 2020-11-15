{-# LANGUAGE OverloadedStrings #-}
module Types 
  ( Handler
  , runHandler
  , runDB
  , run
  , catchErr
  , raiseErr
  , assertTrue
  ) where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Text (Text)
import Data.Time
import Discord
import Discord.Internal.Rest.Prelude
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Database

type Handler a = ExceptT Text (ReaderT DiscordHandle IO) a

runHandler :: DiscordHandle -> Handler () -> IO ()
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

run :: (FromJSON a, Request (r a)) => r a -> Handler a
run r = do
  dis <- lift ask
  res <- liftIO $ restCall dis r
  case res of
    Left err -> throwE . T.pack . show $ err
    Right a -> pure a

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