module Buttons
  ( giveRole
  , removeRole
  , buttons
  , buttonHandler
  , runButtonComm
  , ButtonCommError (..)
  ) where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Foldable
import           Data.Maybe
import           Data.Text                 (Text)
import           Database.Persist.Sql
import           Discord.Requests
import           Discord.Types

import           Commands
import           Schema
import           Types
import           Util

-- | Assign a role to a given guild member.
giveRole :: MonadDiscord m => RoleId -> UserId -> GuildId -> m ()
giveRole role user gid = run $ AddGuildMemberRole gid user role

-- | Revoke a role from a given guild member.
removeRole :: MonadDiscord m => RoleId -> UserId -> GuildId -> m ()
removeRole role user gid = run $ RemoveGuildMemberRole gid user role

-- | Retrieve a list of the active buttons.
buttons :: Handler [Button]
buttons = fmap (map entityVal) $ runDB $ selectList [] []

-- | Handle a button press.
buttonHandler :: ReactionInfo -> Handler ()
buttonHandler rinfo = void . runMaybeT $ do
  myUid <- myUserId
  guard $ myUid /= reactionUserId rinfo
  gid <- MaybeT . pure $ reactionGuildId rinfo
  logS $ "User " <> show (reactionUserId rinfo) <> " reacted with " <> show (emojiName $ reactionEmoji rinfo) <> " on message " <> show (reactionMessageId rinfo)
  mem <- run $ GetGuildMember gid (reactionUserId rinfo)
  btns <- lift buttons
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

withRoleAndChannel :: MonadDiscord m => GuildId -> Text -> Text -> (RoleId -> ChannelId -> m a) -> m (Either ButtonCommError a)
withRoleAndChannel gid rName chanName f = do
  mrid <- tryGetRoleByName gid rName
  case mrid of
    Left err -> pure $ Left (RoleIdNameError err)
    Right rid -> do
      mchan <- tryGetChannelByName gid chanName
      case mchan of
        Left err   -> pure $ Left (ChannelIdNameError err)
        Right chan -> Right <$> f rid chan
