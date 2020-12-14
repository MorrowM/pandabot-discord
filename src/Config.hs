{-# LANGUAGE OverloadedStrings #-}
module Config
( App (..)
, Config (..)
, parseConfigFile
) where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Except (withExceptT, throwE, ExceptT)
import Data.ConfigFile ( emptyCP, readfile, Get_C(get) )
import Data.Text (pack, Text)
import Discord (DiscordHandle)
import Discord.Types (RoleId)
import Text.Read (readMaybe)

data App = App
  { appDis :: DiscordHandle
  , appConfig :: Config
  }

data Config = Config
  { botToken :: Text
  , welcomeRole :: RoleId
  , pointAssignEmoji :: Text
  }

parseConfigFile :: FilePath -> ExceptT Text IO Config
parseConfigFile file = do
  (tok, welcomeRoleTxt, notifEmoji) <- withExceptT (pack . show) $ do
    cp <- join $ liftIO $ readfile emptyCP file
    tok <- pack <$> get cp "DEFAULT" "bot-token"
    welcomeRoleTxt <- get cp "DEFAULT" "welcome-role"
    notifEmoji <- pack <$> get cp "DEFAULT" "notifpoints-emoji"
    pure (tok, welcomeRoleTxt, notifEmoji)
  rid <- maybe (throwE "error parsing welcome-role from configuration") pure (readMaybe welcomeRoleTxt)
  pure $ Config { welcomeRole = rid, botToken = tok, pointAssignEmoji = notifEmoji }