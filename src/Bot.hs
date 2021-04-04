module Bot
( onStart
, eventHandler
) where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Maybe
import           Data.List
import qualified Data.Text                      as T
import           Database.Persist.Sql
import           Discord
import           Discord.Requests
import           Discord.Types
import           Options.Applicative
import           Options.Applicative.Help.Chunk
import           System.Exit

import           Buttons
import           Commands
import           Control.Lens
import           Points
import           Schema
import           Snappers
import           Types
import           Util

-- | Initialize the bot.
onStart :: Handler ()
onStart = catchErr $ do
  runDB $ runMigration migrateAll
  dis <- getDis
  logS . ("Connected as " <>)
    . show . userName . _currentUser =<< liftIO (readCache dis)
  liftIO . void . forkIO $ checkForSnapshots dis

-- | Handle Discord gateway events.
eventHandler :: Event -> Handler ()
eventHandler event = case event of
  GuildMemberAdd gid mem -> do
    let uid = userId $ memberUser mem
    logS $ "User " <> show uid <> " joined guild " <> show gid
    addPandaRole uid gid
  TypingStart _ -> pure ()
  PresenceUpdate _ -> pure ()
  Ready {} -> pure ()
  GuildCreate {} -> pure ()
  MessageReactionAdd rinfo -> buttonHandler rinfo *> handlePointAssign rinfo
  MessageReactionRemove rinfo -> handlePointRemove rinfo
  MessageCreate msg -> handleMessageCreate msg
  other -> logS . head . words . show $ other

handleMessageCreate :: Message -> Handler ()
handleMessageCreate msg = catchErr $ do
  handleComm msg

handleComm :: Message -> Handler ()
handleComm msg =
  catchErr $ do
    if "!help" `T.isPrefixOf` messageText msg
      then runComm ["-h"] msg
      else
        if "!" `T.isPrefixOf` messageText msg
          then
            let args = map T.unpack $ wordsWithQuotes (T.drop 1 (messageText msg))
             in runComm args msg
          else pure ()

runComm :: [String] -> Message -> Handler ()
runComm args msg = void . runMaybeT $ do
  gid <- MaybeT . pure $ messageGuild msg
  mem <- run $ GetGuildMember gid (userId $ messageAuthor msg)
  usrIsAdmin <- isAdmin gid mem
  case execParserPure defaultPrefs (rootComm usrIsAdmin) args of
    Success whichComm -> case whichComm of
      ButtonComm comm ->
        if usrIsAdmin then do
          res <- lift $ runButtonComm comm gid
          case res of
            Right () -> reactPositive
            Left err -> do
              reactNegative
              case err of
                RoleIdNameError NameNotFound -> reply "Sorry, I couldn't find a role with this name."
                RoleIdNameError (NameAmbiguous roles) -> reply $ "There are multiple roles with this name: " <> T.pack (show $ roleId <$> roles)
                ChannelIdNameError NameNotFound -> reply "Sorry, I couldn't find a channel with this name."
                ChannelIdNameError (NameAmbiguous chans) -> reply $ "There are multiple channels with this name: " <> T.pack (show $ snd <$> chans)
          else do
            reactNegative
            reply "You must be an administrator to run this command."
      PointsComm comm -> do
        res <- lift $ runPointsComm comm gid (messageAuthor msg) reply
        case res of
          Right () -> reactPositive
          Left _ -> do
            reactNegative
            reply "Sorry, an unknown error has occured while handling your request"

      LeaderboardComm comm -> do
        res <- lift $ runLeaderboardComm comm gid reply
        case res of
          Right () -> reactPositive
          Left _ -> do
            reactNegative
            reply "Sorry, an unknown error has occured while handling your request"

    Failure f -> do
      let (hlp, status, _) = execFailure f ""
          helpStr = "```" ++ show hlp ++ "```"
      guard $
        not ("Usage:  COMMAND\n" `isPrefixOf` maybe "" show (unChunk $ helpUsage hlp))
        || status == ExitSuccess

      if status == ExitSuccess
        then reactPositive
        else reactNegative

      reply $ T.pack helpStr
    _ -> pure ()
  where
    reply :: MonadDiscord m => T.Text -> m ()
    reply = run_ . CreateMessage (messageChannel msg)

    reactPositive :: (MonadDiscord m, MonadReader App m) => m ()
    reactPositive = do
      emo <- view #reactPositiveEmoji <$> getConfig
      liftIO $ print emo
      run $ CreateReaction (messageChannel msg, messageId msg) emo
    reactNegative = run $ CreateReaction (messageChannel msg, messageId msg) ":x:"


addPandaRole :: UserId -> GuildId -> Handler ()
addPandaRole usr gid = do
  welcome <-  view #welcomeRole <$> getConfig
  run $ AddGuildMemberRole gid usr welcome

