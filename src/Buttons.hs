module Buttons
  ( giveRole
  , removeRole
  , buttons
  , buttonHandler
  , runButtonComm
  , ButtonCommError (..)
  ) where

import           Control.Monad        (when)
import           Data.Foldable        (for_)
import           Data.Maybe           (isNothing)
import           Data.Text            (Text)
import           Database.Persist.Sql (Entity (entityVal),
                                       PersistQueryRead (selectFirst),
                                       PersistQueryWrite (deleteWhere),
                                       PersistStoreWrite (insert), selectList,
                                       (==.))
import           Discord.Requests     (ChannelRequest (CreateMessage, CreateReaction, DeleteOwnReaction, DeleteUserReaction),
                                       GuildRequest (AddGuildMemberRole, GetGuildMember, RemoveGuildMemberRole))
import           Discord.Types        (ChannelId, Emoji (emojiName), GuildId,
                                       GuildMember (memberRoles),
                                       Message (messageId),
                                       ReactionInfo (reactionEmoji, reactionGuildId, reactionMessageId, reactionUserId),
                                       Role, RoleId, UserId)

import           Commands             (ButtonComm (..))
import           Schema               (Button (..),
                                       EntityField (ButtonChannel, ButtonEmoji, ButtonMessage, ButtonRole))
import           Types                (Handler, NameError, assertTrue, run_,
                                       run, runDB, runDB_)
import           Util                 (inGuild, logS, myUserId, stripEmoji,
                                       tryGetChannelByName, tryGetRoleByName)

-- | Assign a role to a given guild member.
giveRole :: RoleId -> UserId -> GuildId -> Handler ()
giveRole role user gid = run $ AddGuildMemberRole gid user role

-- | Revoke a role from a given guild member.
removeRole :: RoleId -> UserId -> GuildId -> Handler ()
removeRole role user gid = run $ RemoveGuildMemberRole gid user role

-- | Retrieve a list of the active buttons.
buttons :: Handler [Button]
buttons = fmap (map entityVal) $ runDB $ selectList [] []

-- | Handle a button press.
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

-- | Handle invokations of the button command.
runButtonComm :: ButtonComm -> GuildId -> Handler (Either ButtonCommError ())
runButtonComm btn gid = case btn of
  AddButton chanName emoji rName txt -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      msg <- run $ CreateMessage chan txt
      runDB_ $ insert $ Button chan (messageId msg) emoji rid
      logS $ "Created button in channel " <> show chan <> " on message " <> show (messageId msg) <> " with emoji " <> show emoji <> " for role " <> show rid
      run_ $ CreateReaction (chan, messageId msg) emoji

  InsertButton chanName emoji rName mid -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      runDB_ $ insert $ Button chan mid emoji rid
      logS $ "Inserted button in channel " <> show chan <> " on message " <> show mid <> " with emoji " <> show emoji <> " for role " <> show rid
      run_ $ CreateReaction (chan, mid) emoji

  RemoveButton chanName emoji rName mid -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      runDB_ $ deleteWhere [ButtonChannel ==. chan, ButtonMessage ==. mid, ButtonEmoji ==. emoji, ButtonRole ==. rid]
      remainingRoles <- runDB $ selectFirst [ButtonChannel ==. chan, ButtonMessage ==. mid, ButtonEmoji ==. emoji] []
      when (isNothing remainingRoles) $ run_ $ DeleteOwnReaction (chan, mid) emoji

-- | Errors that can occur while running a button command.
data ButtonCommError
  = RoleIdNameError (NameError Role)
  | ChannelIdNameError (NameError (Text, ChannelId))

withRoleAndChannel :: GuildId -> Text -> Text -> (RoleId -> ChannelId -> Handler a) -> Handler (Either ButtonCommError a)
withRoleAndChannel gid rName chanName f = do
  mrid <- tryGetRoleByName gid rName
  case mrid of
    Left err -> pure $ Left (RoleIdNameError err)
    Right rid -> do
      mchan <- tryGetChannelByName gid chanName
      case mchan of
        Left err   -> pure $ Left (ChannelIdNameError err)
        Right chan -> Right <$> f rid chan
