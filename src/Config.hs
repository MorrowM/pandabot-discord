{-# LANGUAGE DeriveAnyClass #-}
module Config
  ( App (..),
    Config (..),
    parseConfigFile,
  )
where

import           Control.Monad.Except (ExceptT (..))
import           Data.Aeson
import           Data.Bifunctor
import           Data.Text            (Text, pack)
import           Discord              (DiscordHandle)
import           Discord.Types        (RoleId)
import           GHC.Generics

-- | The application environment.
data App = App
  { appDis    :: DiscordHandle,
    appConfig :: Config
  }

-- | The application configuration
data Config = Config
  { botToken           :: Text,
    welcomeRole        :: RoleId,
    pointAssignEmoji   :: Text,
    reactPositiveEmoji :: Text
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

parseConfigFile :: FilePath -> ExceptT Text IO Config
parseConfigFile path = ExceptT $ first pack <$> eitherDecodeFileStrict path
