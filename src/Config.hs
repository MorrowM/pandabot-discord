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
  , pointsRole :: RoleId
  }

parseConfigFile :: FilePath -> ExceptT Text IO Config
parseConfigFile file = do
  (tok, welcomeRoleTxt, pointsRoleTxt, notifEmoji) <- withExceptT (pack . show . fst) $ do
    cp <- join $ liftIO $ readfile emptyCP file
    tok <- pack <$> get cp "DEFAULT" "bot-token"
    welcomeRoleTxt <- get cp "DEFAULT" "welcome-role"
    pointsRoleTxt <- get cp "DEFAULT" "notifpoints-role"
    notifEmoji <- pack <$> get cp "DEFAULT" "notifpoints-emoji"
    pure (tok, welcomeRoleTxt, pointsRoleTxt, notifEmoji)
  welcomeRid <- maybe (throwE "invalid welcome-role") pure (readMaybe welcomeRoleTxt)
  pointsRid <- maybe (throwE "invalid notifpoints-role") pure (readMaybe pointsRoleTxt)
  pure $ Config 
    { welcomeRole = welcomeRid
    , botToken = tok
    , pointAssignEmoji = notifEmoji
    , pointsRole = pointsRid 
    }