{-# LANGUAGE OverloadedStrings #-}
module NotifPoints 
  ( runNotifPointsComm
  , handlePointAssign
  , NotifPointsCommError(..)
  ) where

import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import Data.Text ( Text, unpack )
import Data.Time ( getCurrentTime )
import Database.Persist.Sql ( (==.), PersistQueryRead(count), insert )
import Discord.Requests ( GuildRequest(..), ChannelRequest(..) )
import Discord.Types ( GuildId, User(userId, userName), ReactionInfo (..), Emoji(..), Message(..) )

import Commands ( NotifPointsComm(..) )
import Config ( Config(..) )
import Schema
    ( EntityField(NotifPointGuild, NotifPointAssignedTo), NotifPoint(..) )
import Types ( runDB, execDB, Handler, run, catchErr, assertJust, getConfig )
import Util ( tshow, isAdmin, logS )

runNotifPointsComm :: NotifPointsComm -> GuildId -> User -> (Text -> Handler ()) -> Handler (Either NotifPointsCommError ())
runNotifPointsComm comm gid usr reply = do
  case comm of
    ViewSelf -> do
      points <- runDB $ count [NotifPointAssignedTo ==. userId usr, NotifPointGuild ==. gid]
      reply $ userName usr <> " has " <> tshow points <> " points"
      pure $ Right ()

data NotifPointsCommError

handlePointAssign :: ReactionInfo -> Handler ()
handlePointAssign rinfo = catchErr $ do
  gid <- assertJust $ reactionGuildId rinfo
  mem <- run $ GetGuildMember gid (reactionUserId rinfo)
  admin <- isAdmin gid mem
  npEmoji <- pointAssignEmoji <$> getConfig
  msg <- run $ GetChannelMessage (reactionChannelId rinfo, reactionMessageId rinfo)
  npRole <- pointsRole <$> getConfig
  let roleIsPinged = npRole `elem` messageMentionRoles msg
  when (admin && emojiName (reactionEmoji rinfo) == npEmoji && roleIsPinged) $ do
    time <- liftIO getCurrentTime
    execDB $ insert (NotifPoint (reactionMessageId rinfo) gid (reactionUserId rinfo) (userId $ messageAuthor msg) time)
    logS $ "Awarded one point to " <> unpack (userName $ messageAuthor msg) <> " for their message " <> show (messageId msg)