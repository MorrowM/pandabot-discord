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
  } deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config

-- | A record of which replies have been made for which message,
-- in order to be able to delete them when necessary.
newtype PointMessages = PointMessages
  { messages :: Map (Snowflake User, Snowflake Message) Message
  } deriving (Show, Eq, Generic)
