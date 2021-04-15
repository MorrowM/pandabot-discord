module Pandabot.Handlers
( registerEventHandlers
) where

import           Calamity
import           Calamity.Cache.Eff
import           Calamity.Commands    as C
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Flags
import           Data.Foldable
import qualified Data.Map             as Map
import qualified Data.Text.Lazy       as L
import           Data.Time
import qualified Data.Vector.Unboxing as V
import           Database.Persist     as DB
import qualified Polysemy             as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Fail        as P
import qualified Polysemy.NonDet      as P
import qualified Polysemy.Reader      as P
import           TextShow

import           Pandabot.Database
import           Pandabot.Hotfix
import           Pandabot.Schema
import           Pandabot.Types
import           Pandabot.Util

-- | Register the various event handlers for the bot.
registerEventHandlers ::
  ( BotC r
  , P.Member Persistable r
  , P.Member P.Fail r
  , P.Member (P.Reader Config) r
  , P.Member (P.AtomicState PointMessages) r)
  => P.Sem r ()
registerEventHandlers = do
  void $ react @('CustomEvt "command-error" (C.Context, CommandError)) $ \(ctx, e) -> do
        info $ "Command failed with reason: " <> showtl e
        case e of
          ParseError n r -> void . tellt ctx $ "Failed to parse parameter: `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
          CheckError n r -> void . tellt ctx $ "Failed to pass a check: `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
          InvokeError n r -> void . tellt ctx $ "Failed to invoke command `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
        let msg = ctx ^. #message
        void . invoke $ CreateReaction msg msg (UnicodeEmoji "❌")
  void $ react @('CustomEvt "command-invoked" C.Context) $ \ctx -> do
    emoj <- view #reactPositiveEmoji <$> P.ask @Config
    let msg = ctx ^. #message
    void . invoke $ HF $ CreateReaction msg msg emoj

  void $ P.runNonDetMaybe $ react @'RawMessageReactionAddEvt $ \rinfo -> do
    Just gid <- pure (rinfo ^. #guildID)
    Just myUid <- fmap getID <$> getBotUser
    guard $ myUid /= (rinfo ^. #userID)
    btns <- fmap (fmap entityVal) $ db
      $ selectList [ ButtonMessage ==. (rinfo ^. #messageID)
                   , ButtonEmoji ==. showt (rinfo ^. #emoji)
                   ] []
    for_ btns $ \btn -> do
      Right mem <- invoke $ GetGuildMember gid (rinfo ^. #userID)
      void $ if buttonRole btn `V.elem` (mem ^. #roles)
        then invoke $ RemoveGuildMemberRole gid (mem ^. #id) (buttonRole btn)
        else invoke $ AddGuildMemberRole    gid (mem ^. #id) (buttonRole btn)
      invoke $ DeleteUserReaction (rinfo ^. #channelID) (rinfo ^. #messageID) (rinfo ^. #emoji) mem

  void $ react @'GuildMemberAddEvt $ \mem -> do
    info $ "User " <> (mem ^. #username) <> " joined guild " <> (mem ^. #guildID . to showt . lazy)
    wrole <- view #welcomeRole <$> P.ask @Config
    void . invoke $ AddGuildMemberRole (mem ^. #guildID) (mem ^. #id) wrole

  void $ P.runNonDetMaybe $ react @'MessageReactionAddEvt $ \(msg, rct) -> do
    npEmoji <- view #pointAssignEmoji <$> P.ask @Config
    guard $ npEmoji == rct ^. #emoji
    Just gid <- pure (rct ^. #guildID)
    Right mem <- invoke $ GetGuildMember gid (rct ^. #userID)
    perms <- permissionsIn' gid mem
    guard $ perms `containsAll` administrator
    time <- P.embed getCurrentTime
    db_ $ insert (Point (msg ^.  #id) gid (rct ^. #userID) (msg ^. #author) time)
    points <- db $ DB.count [PointAssignedTo ==. (msg ^. #author), PointGuild ==. gid]
    Right awardMsg <- invoke
      . CreateMessage msg
      $ def & #content ?~
        msg ^. #author . to mention . strict
        <> " has been awarded a bamboo shoot for being an awesome panda!\nThey now have "
        <> showPoints points  <> " total."
    P.atomicModify' @PointMessages $ #messages %~ Map.insert (rct ^. #userID, msg ^. #id) awardMsg
  void $ react @'MessageReactionRemoveEvt $ \(_msg, rct) -> do
    pointEmoji <- view #pointAssignEmoji <$> P.ask @Config
    when (pointEmoji == rct ^. #emoji) $ do
      db_ $ deleteWhere
        [ PointMessage ==. (rct ^. #messageID)
        , PointAssignedBy ==. (rct ^. #userID)
        ]
      msgs <- P.atomicGet @PointMessages
      let mmsg = msgs ^. #messages . to (Map.lookup (rct ^. #userID, rct ^. #messageID))
      case mmsg of
        Nothing -> pure ()
        Just msg -> do
          void . invoke $ DeleteMessage msg msg
          P.atomicModify' @PointMessages $ #messages %~ Map.delete (rct ^. #userID, rct ^. #messageID)


