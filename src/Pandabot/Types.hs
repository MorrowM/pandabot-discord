module Pandabot.Types
  ( Config (..)
  , PointMessages (..)
  ) where

import           Calamity
import           Data.Aeson
import           Data.Generics.Labels ()
import           Data.Map             (Map)
import           Data.Text            (Text)
import           GHC.Generics

-- | The application configuration
data Config = Config
  { botToken           :: Text
  , welcomeRole        :: Snowflake Role
  , pointAssignEmoji   :: RawEmoji
  , reactPositiveEmoji :: RawEmoji
  , commandPrefix      :: Text
  , connectionString   :: Text
  , voiceConfig        :: VoiceConfig
  } deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config

newtype VoiceConfig = VoiceConfig
  { roles :: [VoiceRole]
  } deriving (Show, Eq, Generic)

instance FromJSON VoiceConfig
instance ToJSON VoiceConfig

data VoiceRole = VoiceRole
  { name          :: Text
  , role          :: Snowflake Role
  , voiceChannels :: [Snowflake VoiceChannel]
  , textChannel   :: Maybe (Snowflake TextChannel)
  } deriving (Show, Eq, Generic)

instance FromJSON VoiceRole
instance ToJSON VoiceRole

-- | A record of which replies have been made for which message,
-- in order to be able to delete them when necessary.
newtype PointMessages = PointMessages
  { messages :: Map (Snowflake User, Snowflake Message) Message
  } deriving (Show, Eq, Generic)
