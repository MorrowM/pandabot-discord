module Types where

import           Calamity
import           Data.Aeson
import           Data.Generics.Labels ()
import           Data.Text            (Text)
import           GHC.Generics

-- | The application configuration
data Config = Config
  { botToken           :: Text
  , welcomeRole        :: Snowflake Role
  , pointAssignEmoji   :: Text
  , reactPositiveEmoji :: Text
  , commandPrefix      :: Text
  , connectionString   :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON Config
instance ToJSON Config
