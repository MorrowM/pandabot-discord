module Points
  ( runPointsComm
  , runLeaderboardComm
  , handlePointAssign
  , handlePointRemove
  , PointsCommError
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           Data.Ord
import           Data.Text              (Text, unpack)
import qualified Data.Text              as T
import           Data.Time
import           Data.Traversable
import           Database.Persist.Sql   as P
import           Discord.Requests
import           Discord.Types

import           Commands
import           Control.Concurrent
import           Control.Lens
import qualified Data.Map               as Map
import           Schema
import           Types
import           Util

-- | Handle invokations of the points command.
runPointsComm :: PointsComm -> GuildId -> User -> (Text -> Handler ()) -> Handler (Either PointsCommError ())
runPointsComm comm gid usr reply = do
  case comm of
    ViewSelf -> do
      points <- runDB $ count [PointAssignedTo ==. userId usr, PointGuild ==. gid]
      reply $ userName usr <> " has " <> showPoints points <> "."
      pure $ Right ()

data PointsCommError

handlePointAssign :: ReactionInfo -> Handler ()
handlePointAssign rinfo = catchErr $ do
  gid <- assertJust $ reactionGuildId rinfo
  mem <- run $ GetGuildMember gid (reactionUserId rinfo)
  admin <- isAdmin gid mem
  npEmoji <- view $ #config . #pointAssignEmoji
  msg <- run $ GetChannelMessage (reactionChannelId rinfo, reactionMessageId rinfo)
  when (admin && emojiName (reactionEmoji rinfo) == npEmoji) $ do
    time <- liftIO getCurrentTime
    runDB_ $ P.insert (Point (reactionMessageId rinfo) gid (reactionUserId rinfo) (userId $ messageAuthor msg) time)
    points <- runDB $ count [PointAssignedTo ==. userId (messageAuthor msg), PointGuild ==. gid]
    awardMsg <- run
      . CreateMessage (messageChannel msg)
      $ "<@" <> tshow (userId $ messageAuthor msg)
        <> "> has been awarded a bamboo shoot for being an awesome panda!\nThey now have " <> showPoints points  <> " total."

    logS $ "Awarded one point to " <> unpack (userName $ messageAuthor msg) <> " for their message " <> show (messageId msg)
    cache <- view  #cache
    liftIO $ modifyMVar_ cache (pure . over #pointAwardMessages (Map.insert rinfo (messageChannel awardMsg, messageId awardMsg)))

handlePointRemove :: ReactionInfo -> Handler ()
handlePointRemove rinfo = catchErr $ do
  pointEmoji <- view $ #config . #pointAssignEmoji
  when (pointEmoji == emojiName (reactionEmoji rinfo)) $ do
    runDB_ $ deleteWhere
      [ PointMessage ==. reactionMessageId rinfo
      , PointAssignedBy ==. reactionUserId rinfo
      ]
    cache <- view #cache
    mmsg <- liftIO $ Map.lookup rinfo . view #pointAwardMessages <$> readMVar cache
    case mmsg of
      Nothing -> pure ()
      Just msg -> do
        run_ $ DeleteMessage msg
        liftIO $ modifyMVar_ cache (pure . over #pointAwardMessages (Map.delete rinfo))

runLeaderboardComm :: LeaderboardComm -> GuildId -> (Text -> Handler ()) -> Handler (Either LeaderboardCommError ())
runLeaderboardComm _ gid reply = do
    pointsRaw <- runDB $ selectList [PointGuild ==. gid] [Asc PointAssignedTo]
    let pointsList = take 5 $ sortOn (Down . snd) $ map (\xs -> (head xs, length xs)) . group $ pointAssignedTo . entityVal <$> pointsRaw
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
