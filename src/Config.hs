module Config
  ( App (..),
    Config (..),
    parseConfigFile,
  )
where

import           Control.Monad          (join)
import           Control.Monad.Except   (ExceptT, MonadError, throwError,
                                         withExceptT)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.ConfigFile        (Get_C (get), emptyCP, readfile)
import           Data.Text              (Text, pack)
import           Discord                (DiscordHandle)
import           Discord.Types          (Emoji, RoleId)
import           Text.Read              (readMaybe)

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
    pointsRole         :: RoleId,
    reactPositiveEmoji :: Text
  }

-- | Parse the configration file into a program configuration.
parseConfigFile :: FilePath -> ExceptT Text IO Config
parseConfigFile file = do
  (tok, welcomeRoleTxt, pointsRoleTxt, notifEmoji, rctPositiveEmoji) <- withExceptT (pack . show . fst) $ do
    cp <- join $ liftIO $ readfile emptyCP file
    tok <- pack <$> get cp "DEFAULT" "bot-token"
    welcomeRoleTxt <- get cp "DEFAULT" "welcome-role"
    pointsRoleTxt <- get cp "DEFAULT" "notifpoints-role"
    notifEmoji <- pack <$> get cp "DEFAULT" "notifpoints-emoji"
    rctPositiveEmoji <- pack <$> get cp "DEFAULT" "reactpositive-emoji"
    pure (tok, welcomeRoleTxt, pointsRoleTxt, notifEmoji, rctPositiveEmoji)
  welcomeRid <- maybe (throwError "invalid welcome-role") pure (readMaybe welcomeRoleTxt)
  pointsRid <- maybe (throwError "invalid notifpoints-role") pure (readMaybe pointsRoleTxt)
  pure $
    Config
      { welcomeRole = welcomeRid,
        botToken = tok,
        pointAssignEmoji = notifEmoji,
        pointsRole = pointsRid,
        reactPositiveEmoji = rctPositiveEmoji
      }
