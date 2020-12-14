{-# LANGUAGE OverloadedStrings #-}

module Bot 
( onStart
, eventHandler
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Bits
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Discord
import Discord.Requests
import Discord.Types
import Options.Applicative
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Persist.Sql
import System.Exit
import Text.Read ( readMaybe )

import Buttons
import Commands
import Schema
import Types

import Options.Applicative.Help.Chunk

onStart :: Handler ()
onStart = catchErr $ do
  runDB $ runMigration migrateAll
  dis <- lift ask
  cache <- liftIO $ readCache dis
  liftIO $ putStrLn $ "Connected as " <> show (userName $ _currentUser cache)

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
  MessageReactionAdd rinfo -> buttonHandler rinfo
  MessageCreate msg -> handleMessageCreate msg
  other -> logS . head . words . show $ other

buttonHandler :: ReactionInfo -> Handler ()
buttonHandler rinfo = do
  myUid <- myUserId
  assertTrue $ myUid /= reactionUserId rinfo
  inGuild (reactionGuildId rinfo) $ \gid -> do
    logS $ "User " <> show (reactionUserId rinfo) <> " reacted with " <> show (emojiName $ reactionEmoji rinfo) <> " on message " <> show (reactionMessageId rinfo)
    mem <- run $ GetGuildMember gid (reactionUserId rinfo)
    btns <- buttons
    for_ btns $ \button -> do
      let bmsg = buttonMessage button
          bemoji = buttonEmoji button
          bchannel = buttonChannel button
          brole = buttonRole button
      when (bmsg == reactionMessageId rinfo && emojiName (reactionEmoji rinfo) == stripEmoji bemoji) $ do
        if brole `elem` memberRoles mem
          then removeRole brole (reactionUserId rinfo) gid
          else giveRole brole (reactionUserId rinfo) gid
        run $ DeleteUserReaction (bchannel, bmsg) (reactionUserId rinfo) bemoji
        logS $ "User " <> show (reactionUserId rinfo) <> " pressed the button " <> show bemoji <> " on message " <> show bmsg

handleMessageCreate :: Message -> Handler ()
handleMessageCreate msg = catchErr $ do
  handleComm msg

handleComm :: Message -> Handler ()
handleComm msg =
  catchErr $
    if "!help" `T.isPrefixOf` messageText msg
      then runComm ["-h"] msg
      else
        if "!" `T.isPrefixOf` messageText msg
          then
            let args = map T.unpack $ wordsWithQuotes (T.drop 1 (messageText msg))
             in runComm args msg
          else pure ()

runComm :: [String] -> Message -> Handler ()
runComm args msg = catchErr $ case execParserPure defaultPrefs rootComm args of
  Success whichComm -> case whichComm of
    ButtonComm comm -> inGuild (messageGuild msg) $ \gid -> do
      mem <- run $ GetGuildMember gid (userId $ messageAuthor msg)
      usrIsAdmin <- isAdmin gid mem
      
      if usrIsAdmin then do
        res <- runButtonComm comm gid
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
    NotifPointsComm comm -> inGuild (messageGuild msg) $ \gid -> do
      res <- runNotifPointsComm comm gid (messageAuthor msg) reply
      case res of
        Right () -> reactPositive
        Left _ -> do
          reactNegative
          reply "Sorry, an unknown error has occured while handling your request"

  Failure f -> do
    let (hlp, status, _) = execFailure f ""
        helpStr = "```" ++ show hlp ++ "```"
    assertTrue $ 
      not ("Usage:  COMMAND\n" `isPrefixOf` maybe "" show (unChunk $ helpUsage hlp))
      || status == ExitSuccess

    if status == ExitSuccess
      then reactPositive
      else reactNegative

    reply $ T.pack helpStr
  _ -> pure ()
  where
    reply = exec . CreateMessage (messageChannel msg)
    reactPositive = run $ CreateReaction (messageChannel msg, messageId msg) ":white_check_mark:"
    reactNegative = run $ CreateReaction (messageChannel msg, messageId msg) ":x:"

data ButtonCommError 
  = RoleIdNameError (NameError Role) 
  | ChannelIdNameError (NameError (Text, ChannelId))

runButtonComm :: ButtonComm -> GuildId -> Handler (Either ButtonCommError ())
runButtonComm btn gid = case btn of
  AddButton chanName emoji rName txt -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      msg <- run $ CreateMessage chan txt
      execDB $ P.insert $ Button chan (messageId msg) emoji rid
      logS $ "Created button in channel " <> show chan <> " on message " <> show (messageId msg) <> " with emoji " <> show emoji <> " for role " <> show rid
      exec $ CreateReaction (chan, messageId msg) emoji
    
  InsertButton chanName emoji rName mid -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      execDB $ P.insert $ Button chan mid emoji rid
      logS $ "Inserted button in channel " <> show chan <> " on message " <> show mid <> " with emoji " <> show emoji <> " for role " <> show rid
      exec $ CreateReaction (chan, mid) emoji

  RemoveButton chanName emoji rName mid -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      execDB $ P.deleteWhere [ButtonChannel ==. chan, ButtonMessage ==. mid, ButtonEmoji ==. emoji, ButtonRole ==. rid]
      remainingRoles <- runDB $ P.selectFirst [ButtonChannel ==. chan, ButtonMessage ==. mid, ButtonEmoji ==. emoji] []
      when (isNothing remainingRoles) $ exec $ DeleteOwnReaction (chan, mid) emoji

runNotifPointsComm :: NotifPointsComm -> GuildId -> User -> (Text -> Handler ()) -> Handler (Either NotifPointsCommError ())
runNotifPointsComm comm gid usr reply = do
  case comm of
    ViewSelf -> do
      points <- runDB $ count [NotifPointAssignedTo P.==. userId usr, NotifPointGuild P.==. gid]
      reply $ userName usr <> " has " <> tshow points <> " points"
      pure $ Right ()

data NotifPointsCommError

withRoleAndChannel :: GuildId -> Text -> Text -> (RoleId -> ChannelId -> Handler a) -> Handler (Either ButtonCommError a)
withRoleAndChannel gid rName chanName f = do
  mrid <- tryGetRoleByName gid rName
  case mrid of
    Left err -> pure $ Left (RoleIdNameError err)
    Right rid -> do
      mchan <- tryGetChannelByName gid chanName
      case mchan of
        Left err -> pure $ Left (ChannelIdNameError err)
        Right chan -> Right <$> f rid chan

logS :: MonadIO m => String -> m ()
logS s = liftIO $ do
  t <- getCurrentTime
  let fmt = formatTime defaultTimeLocale "[%F %T] " t
      output = fmt <> s <> "\n"
  putStr output
  appendFile "log.txt" output

addPandaRole :: UserId -> GuildId -> Handler ()
addPandaRole usr gid = when (gid == 753002885633146932) $
  run $ AddGuildMemberRole gid usr 762055744555188234 -- Hardcoded! TODO Change this

wordsWithQuotes :: Text -> [Text]
wordsWithQuotes = concat . wordsEveryOther . T.splitOn "\""
  where
    wordsEveryOther :: [Text] -> [[Text]]
    wordsEveryOther [] = []
    wordsEveryOther [z] = [T.words z]
    wordsEveryOther (x : y : xs) = T.words x : [y] : wordsEveryOther xs

myUserId :: Handler UserId
myUserId = do
  dis <- lift ask
  cache <- liftIO $ readCache dis
  pure $ userId $ _currentUser cache

stripEmoji :: Text -> Text
stripEmoji emoji = if T.all isAscii emoji
  then T.takeWhile (/= ':') . T.drop 2 $ emoji
  else emoji

inGuild :: Maybe GuildId -> (GuildId -> Handler ()) -> Handler ()
inGuild = flip $ maybe (pure ())

data NameError a = NameNotFound | NameAmbiguous [a]

tryGetRoleByName :: GuildId -> Text -> Handler (Either (NameError Role) RoleId)
tryGetRoleByName gid name = do
  roles <- run $ GetGuildRoles gid
  pure $ tryGetIdByName roles roleName roleId name

tryGetChannelByName :: GuildId -> Text -> Handler (Either (NameError (Text, ChannelId)) ChannelId)
tryGetChannelByName gid name = do
  mchans <- run $ GetGuildChannels gid
  let chans = catMaybes $ isText <$> mchans
  pure $ tryGetIdByName chans fst snd name
  where 
    isText chan = case chan of
      ChannelText {} -> Just (channelName chan, channelId chan)
      _ -> Nothing

tryGetIdByName :: [a] -> ( a -> Text) -> (a -> Snowflake) -> Text -> Either (NameError a) Snowflake
tryGetIdByName vals toText toId name = case filter ((==name) . toText) vals of
  [] -> case readMaybe (T.unpack name) :: Maybe Snowflake of
    Nothing -> Left NameNotFound
    Just flake -> Right flake
  [x] -> Right $ toId x
  xs -> Left (NameAmbiguous xs)

isAdmin :: GuildId -> GuildMember -> Handler Bool
isAdmin gid mem = do
  roles <- run $ GetGuildRoles gid
  pure $ any (`elem` memberRoles mem) (roleId <$> filter isAdminRole roles)
  where
    isSet b n = (b .&. n) == b
    isAdminRole = isSet 8 . rolePerms

tshow :: Show a => a -> Text
tshow = T.pack . show