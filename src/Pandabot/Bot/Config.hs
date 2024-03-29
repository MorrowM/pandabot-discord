{-# LANGUAGE DuplicateRecordFields #-}

module Pandabot.Bot.Config
  ( Config(..)
  , CLIOptions(..)
  ) where

import           Calamity
import           Data.Aeson
import           GHC.Generics
import           Optics
import           Options.Generic

-- | CLI options
newtype CLIOptions w = Options
  { config :: w ::: Maybe FilePath <?> "The path to the configuration file (must end with .json/.yaml)"
  }
  deriving (Generic)

instance ParseRecord (CLIOptions Wrapped)

-- | The application configuration
data Config = Config
  { botToken           :: Text
  , welcomeRole        :: Snowflake Role
  , notifGangRole      :: Snowflake Role
  , pinConfig          :: PinConfig
  , pointAssignEmoji   :: RawEmoji
  , reactPositiveEmoji :: RawEmoji
  , commandPrefix      :: Text
  , connectionString   :: Text
  , allowedGuilds      :: Maybe [Snowflake Guild]
  , voiceConfig        :: VoiceConfig
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VoiceConfig = VoiceConfig
  { roles              :: [VoiceRole]
  , messageDeleteDelay :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data VoiceRole = VoiceRole
  { name          :: Text
  , role          :: Snowflake Role
  , voiceChannels :: [Snowflake VoiceChannel]
  , textChannel   :: Maybe (Snowflake TextChannel)
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PinConfig = PinConfig
  { emoji :: RawEmoji
  , roles :: [Snowflake Role]
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

makeFieldLabelsNoPrefix ''Config
makeFieldLabelsNoPrefix ''VoiceConfig
makeFieldLabelsNoPrefix ''VoiceRole
makeFieldLabelsNoPrefix ''PinConfig
