module Buttons 
  ( giveRole
  , removeRole
  , buttons
  , buttonHandler
  , runButtonComm
  , ButtonCommError (..)
  ) where

import Control.Monad ( when )
import Data.Foldable ( for_ )
import Data.Maybe ( isNothing )
import Data.Text (Text)
import Database.Persist.Sql
    ( (==.),
      selectList,
      Entity(entityVal),
      PersistQueryRead(selectFirst),
      PersistQueryWrite(deleteWhere),
      PersistStoreWrite(insert) )
import Discord.Requests
    ( ChannelRequest(DeleteOwnReaction, DeleteUserReaction,
                     CreateMessage, CreateReaction),
      GuildRequest(GetGuildMember, AddGuildMemberRole,
                   RemoveGuildMemberRole) )
import Discord.Types
    ( ChannelId,
      GuildId,
      RoleId,
      UserId,
      Message(messageId),
      ReactionInfo(reactionGuildId, reactionMessageId, reactionEmoji,
                   reactionUserId),
      Emoji(emojiName),
      GuildMember(memberRoles),
      Role )

import Commands ( ButtonComm(..) )
import Schema
    ( Button(..),
      EntityField(ButtonEmoji, ButtonRole, ButtonChannel,
                  ButtonMessage) )
import Types
    ( assertTrue, exec, execDB, run, runDB, Handler, NameError )
import Util
    ( myUserId,
      stripEmoji,
      inGuild,
      tryGetRoleByName,
      tryGetChannelByName,
      logS )


giveRole :: RoleId -> UserId -> GuildId -> Handler ()
giveRole role user gid = run $ AddGuildMemberRole gid user role

removeRole :: RoleId -> UserId -> GuildId -> Handler ()
removeRole role user gid = run $ RemoveGuildMemberRole gid user role

buttons :: Handler [Button]
buttons = fmap (map entityVal) $ runDB $ selectList [] []

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

runButtonComm :: ButtonComm -> GuildId -> Handler (Either ButtonCommError ())
runButtonComm btn gid = case btn of
  AddButton chanName emoji rName txt -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      msg <- run $ CreateMessage chan txt
      execDB $ insert $ Button chan (messageId msg) emoji rid
      logS $ "Created button in channel " <> show chan <> " on message " <> show (messageId msg) <> " with emoji " <> show emoji <> " for role " <> show rid
      exec $ CreateReaction (chan, messageId msg) emoji
    
  InsertButton chanName emoji rName mid -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      execDB $ insert $ Button chan mid emoji rid
      logS $ "Inserted button in channel " <> show chan <> " on message " <> show mid <> " with emoji " <> show emoji <> " for role " <> show rid
      exec $ CreateReaction (chan, mid) emoji

  RemoveButton chanName emoji rName mid -> do
    withRoleAndChannel gid rName chanName $ \rid chan -> do
      execDB $ deleteWhere [ButtonChannel ==. chan, ButtonMessage ==. mid, ButtonEmoji ==. emoji, ButtonRole ==. rid]
      remainingRoles <- runDB $ selectFirst [ButtonChannel ==. chan, ButtonMessage ==. mid, ButtonEmoji ==. emoji] []
      when (isNothing remainingRoles) $ exec $ DeleteOwnReaction (chan, mid) emoji


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
        Left err -> pure $ Left (ChannelIdNameError err)
        Right chan -> Right <$> f rid chan