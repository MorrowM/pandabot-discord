{-# LANGUAGE OverloadedStrings #-}
module NotifPoints 
  ( runNotifPointsComm
  , runLeaderboardComm
  , handlePointAssign
  , handlePointRemove
  , NotifPointsCommError
  ) where

import Control.Monad ( when )
import Control.Monad.IO.Class ( liftIO )
import Data.List ( group, sortOn )
import Data.Ord ( Down(..) )
import Data.Text ( Text, unpack )
import qualified Data.Text as T
import Data.Time ( getCurrentTime )
import Data.Traversable ( for )
import Database.Persist.Sql ( (==.), PersistQueryRead(count), insert, deleteWhere, SelectOpt(..), selectList, entityVal )
import Discord.Requests ( GuildRequest(..), ChannelRequest(..) )
import Discord.Types ( GuildId, User(userId, userName), ReactionInfo (..), Emoji(..), Message(..), GuildMember(..) )

import Commands ( NotifPointsComm(..), LeaderboardComm(..) )
import Config ( Config(..) )
import Schema
    ( EntityField(..), NotifPoint(..) )
import Types ( runDB, execDB, Handler, run, exec, catchErr, assertJust, getConfig )
import Util ( tshow, isAdmin, logS )

runNotifPointsComm :: NotifPointsComm -> GuildId -> User -> (Text -> Handler ()) -> Handler (Either NotifPointsCommError ())
runNotifPointsComm comm gid usr reply = do
  case comm of
    ViewSelf -> do
      points <- runDB $ count [NotifPointAssignedTo ==. userId usr, NotifPointGuild ==. gid]
      reply $ userName usr <> " has " <> showPoints points <> "."
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
    points <- runDB $ count [NotifPointAssignedTo ==. userId (messageAuthor msg), NotifPointGuild ==. gid]
    exec $ CreateMessage (messageChannel msg) $ "<@" <> tshow (userId $ messageAuthor msg) 
      <> "> has been awarded a point for notifying the #NotifGang!\nThey now have " <> showPoints points  <> " total."
    logS $ "Awarded one point to " <> unpack (userName $ messageAuthor msg) <> " for their message " <> show (messageId msg)

handlePointRemove :: ReactionInfo -> Handler ()
handlePointRemove rinfo = catchErr $ do
  pointEmoji <- pointAssignEmoji <$> getConfig
  when (pointEmoji == emojiName (reactionEmoji rinfo)) $ execDB $ deleteWhere
    [ NotifPointMessage ==. reactionMessageId rinfo
    , NotifPointAssignedBy ==. reactionUserId rinfo
    ]

runLeaderboardComm :: LeaderboardComm -> GuildId -> (Text -> Handler ()) -> Handler (Either LeaderboardCommError ())
runLeaderboardComm _ gid reply = do
    pointsRaw <- runDB $ selectList [NotifPointGuild ==. gid] [Asc NotifPointAssignedTo]
    let pointsList = take 5 $ sortOn (Down . snd) $ map (\xs -> (head xs, length xs)) . group $ notifPointAssignedTo . entityVal <$> pointsRaw
    points <- for pointsList $ \(uid, len) -> do
      mem <- run $ GetGuildMember gid uid
      pure (userName $ memberUser mem, len)
    if null points
      then reply "No one has any points yet!"
      else reply $ T.unlines [tshow i <> ". " <> name <> ": " <> showPoints p | (i, (name, p)) <- zip [(1 :: Int)..] points]
    pure $ Right ()
    
    
data LeaderboardCommError

showPoints :: Int -> Text
showPoints 0 = "no bamboo shoots"
showPoints 1 = "1 bamboo shoot"
showPoints n = tshow n <> " bamboo shoots"