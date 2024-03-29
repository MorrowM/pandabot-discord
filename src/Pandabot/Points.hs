module Pandabot.Points
  ( MessagePointMessages (..)
  , countPoints
  , registerAwardCommand
  , registerShootsCommand
  , registerLeaderboardCommand
  , awardMessagePoint
  , awardPointMessageText
  , showPoints
  , registerPointGiveHandler
  , registerPointRevokeHandler
  ) where

import           Calamity
import           Data.Map                  (Map)
import           Database.Persist          as DB
import           GHC.Generics              (Generic)
import qualified Polysemy                  as P

import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           Control.Arrow             ((&&&))
import           Control.Monad
import           Data.Default
import           Data.Flags
import           Data.List
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Ord
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Traversable
import qualified Data.Vector.Unboxing      as VU
import           Optics
import qualified Polysemy.AtomicState      as P
import qualified Polysemy.Fail             as P
import qualified Polysemy.NonDet           as P
import qualified Polysemy.Reader           as P
import qualified Polysemy.Time             as P
import           TextShow

import           Pandabot.Bot.Config
import           Pandabot.Bot.Database
import           Pandabot.Bot.Schema
import           Pandabot.Bot.Util

-- | A record of which replies have been made for which message,
-- in order to be able to delete them when necessary.
newtype MessagePointMessages = MessagePointMessages
  { messages :: Map (Snowflake Message) (Message, Int)
  } deriving (Show, Generic)

countPoints ::
  ( BotC r
  , P.Member Persistable r
  , HasID User u
  , HasID Guild g
  ) => u -> g -> P.Sem r Int
countPoints usr gld = (+) <$> msgPoints <*> freePoints
  where
    msgPoints = db $ DB.count [MessagePointAssignedTo ==. getID usr, MessagePointGuild ==. getID gld]
    freePoints = sum . fmap (getFreePointAmnt . entityVal) <$> db (selectList [FreePointAssignedTo ==. getID usr, FreePointGuild ==. getID gld] [])
    getFreePointAmnt (FreePoint _ _ _ _ amnt) = amnt


registerAwardCommand ::
  ( BotC r
  , P.Members
   '[ Persistable
    , P.GhcTime
    ] r
  ) => Check FullContext -> P.Sem (DSLState FullContext r) ()
registerAwardCommand admin = void
    $ P.runNonDetMaybe
    $ requires [admin]
    $ help (const "Award bamboo shoots to pandas.")
    $ hide
    $ command @'[Member, Named "shoots" (Maybe Int), KleenePlusConcat Text] "award" $ \ctx mem mamnt reason -> do
    Just guild <- pure $ ctx ^. #guild
    time <- P.now
    let amnt = fromMaybe 1 mamnt
        point = FreePoint (guild ^. #id) (ctx ^. #user % #id) (mem ^. #id) time amnt
    db_ $ DB.insert point
    points <- countPoints mem guild
    void . invoke $ DeleteMessage ctx ctx
    void . tellt ctx $
      mem ^. to mention
      <> " has been awarded " <> showPoints amnt <> "! " <> reason <> "\nThey now have "
      <> showPoints points  <> " total."


registerShootsCommand ::
  ( BotC r
  , P.Members
   '[ Persistable
    ] r
  ) => P.Sem (DSLState FullContext r) ()
registerShootsCommand = void
    $ help (const "Shows how many bamboo shoots you have in total.")
    $ command @'[Maybe User] "shoots"
    $ \ctx muser -> do
    Just gld <- pure (ctx ^. #guild)
    points <- countPoints (fromMaybe (ctx ^. #user) muser) gld
    void . tellt ctx $ (fromMaybe (ctx ^. #user) muser ^. #username) <> " has " <> showPoints points <> "."

registerLeaderboardCommand ::
  ( BotC r
  , P.Members
   '[ Persistable
    ] r
  ) => P.Sem (DSLState FullContext r) ()
registerLeaderboardCommand = void
    $ help (const "Shows the top bamboo shoot pandas.")
    $ commandA @'[] "leaderboard" ["lb"]
    $ \ctx -> do
    Just gld <- pure (ctx ^. #guild)
    messagePointsRaw <- db $ selectList [MessagePointGuild ==. getID gld] [Asc MessagePointAssignedTo]
    freePointsRaw <- db $ selectList [FreePointGuild ==. getID gld] [Asc FreePointAssignedTo]
    let messagePointsMap
          = Map.fromListWith (+)
          . fmap (,1)
          $ messagePointAssignedTo . entityVal <$> messagePointsRaw
        freePointsMap
          = Map.fromListWith (+)
          $ (freePointAssignedTo &&& freePointAmount) . entityVal <$> freePointsRaw
        pointsMap = Map.unionWith (+) messagePointsMap freePointsMap
        topFive = take 5 . sortOn (Down . snd) . Map.toList $ pointsMap
    points <- for topFive $ \(u, p) -> do
      Just usr <- upgrade u
      pure (usr ^. #username, p)

    let txt = T.unlines [showt i <> ". " <> nm <> ": " <> showPoints p | (i, (nm, p)) <- zip [(1 :: Int)..] points]
    if null points
      then tellt_ ctx "No one has any points yet!"
      else tell_ @Embed ctx $ def
        & #title ?~ "Leaderboard"
        & #description ?~ txt

awardMessagePoint ::
  ( BotC r
  , P.Members
    [ Persistable
    , P.NonDet
    , P.Fail
    , P.AtomicState MessagePointMessages
    , P.GhcTime
    ] r
  ) => Text -> Message -> User -> P.Sem r ()
awardMessagePoint reason msg usr = do
  Just gid <- pure (msg ^. #guildID)
  Right mem <- invoke $ GetGuildMember gid usr
  perms <- permissionsIn' gid mem
  guard $ perms `containsAll` administrator
  time <- P.now
  db_ $ DB.insert (MessagePoint (msg ^.  #id) gid (usr ^. #id) (msg ^. #author % to getID) time)
  points <- countPoints (msg ^. #author) gid
  mawardMsg <- P.atomicGets @MessagePointMessages (view $ #messages % to (Map.lookup $ msg ^. #id))
  case mawardMsg of
    Nothing -> do
      Right awardMsg <- tellt msg $
          awardPointMessageText reason msg 1 points
      P.atomicModify' @MessagePointMessages $ #messages %~ Map.insert (msg ^. #id) (awardMsg, 1)
    Just (awardMsg, amnt) -> do
      void . invoke . EditMessage awardMsg awardMsg . editMessageContent . Just $
          awardPointMessageText reason msg (succ amnt) points
      P.atomicModify' @MessagePointMessages $ #messages %~ Map.adjust (over _2 succ) (msg ^. #id)

awardPointMessageText :: Text -> Message -> Int -> Int -> Text
awardPointMessageText reason msg points total =
  msg ^. #author % to (getID @User) % to mention
  <> " has been awarded " <> showPoints points <> " " <> reason <> "!\nThey now have "
  <> showPoints total  <> " total."

-- | Format a point value to a phrase with proper grammer.
showPoints :: Int -> Text
showPoints 0 = "no bamboo shoots"
showPoints 1 = "1 bamboo shoot"
showPoints n = showt n <> " bamboo shoots"

registerPointGiveHandler ::
  ( BotC r
  , P.Members
    [ Persistable
    , P.Fail
    , P.AtomicState MessagePointMessages
    , P.GhcTime
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerPointGiveHandler = void $ P.runNonDetMaybe $ react @'MessageReactionAddEvt $ \(msg, usr, _chan, rct) -> do
  npEmoji <- P.asks @Config $ view #pointAssignEmoji
  guard $ npEmoji == rct
  notifGangRole <- P.asks @Config $ view #notifGangRole
  let reason
        | notifGangRole `VU.elem` (msg ^. #mentionRoles) = "for notifying the #NotifGang"
        | otherwise = "for being an awesome panda"
  awardMessagePoint reason msg usr

registerPointRevokeHandler ::
  ( BotC r
  , P.Members
    [ Persistable
    , P.Fail
    , P.AtomicState MessagePointMessages
    , P.GhcTime
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerPointRevokeHandler = void $ react @'MessageReactionRemoveEvt $ \(msg, usr, _chan, rct) -> do
  pointEmoji <- P.asks @Config $ view #pointAssignEmoji
  when (pointEmoji == rct) $ do
    db_ $ deleteWhere
      [ MessagePointMessage ==. (msg ^. #id)
      , MessagePointAssignedBy ==. (usr ^. #id)
      ]
    mmsg <- P.atomicGets @MessagePointMessages (view $ #messages % to (Map.lookup (msg ^. #id)))
    case mmsg of
      Nothing -> pure ()
      Just (awardMsg, 1) -> do
        void . invoke $ DeleteMessage awardMsg awardMsg
        P.atomicModify' @MessagePointMessages $ #messages %~ Map.delete (msg ^. #id)
      Just (awardMsg, amnt) -> do
        Just gid <- pure $ msg ^. #guildID
        points <- countPoints (msg ^. #author) gid
        void . invoke . EditMessage awardMsg awardMsg . editMessageContent . Just $
          awardPointMessageText "for being an awesome panda" msg (pred amnt) points -- TODO try to recover the original `reason`
        P.atomicModify' @MessagePointMessages $ #messages %~ Map.adjust (over _2 pred) (msg ^. #id)
