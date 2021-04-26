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
import qualified Data.Vector.Unboxing as V
import           Database.Persist     as DB
import qualified Polysemy             as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Fail        as P
import qualified Polysemy.NonDet      as P
import qualified Polysemy.Reader      as P
import qualified Polysemy.Time        as P
import           TextShow

import           Pandabot.Commands
import           Pandabot.Database
import           Pandabot.Schema
import           Pandabot.Types
import           Pandabot.Util

-- | Register the various event handlers for the bot.
registerEventHandlers ::
  ( BotC r
  , P.Members
    [ Persistable
    , P.Fail
    , P.Reader Config
    , P.AtomicState MessagePointMessages
    , P.GhcTime
    ] r )
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
    emoj <- P.asks @Config $ view #reactPositiveEmoji
    let msg = ctx ^. #message
    void . invoke $ CreateReaction msg msg emoj

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
    wrole <- P.asks @Config $ view #welcomeRole
    void . invoke $ AddGuildMemberRole (mem ^. #guildID) (mem ^. #id) wrole

  void $ P.runNonDetMaybe $ react @'MessageReactionAddEvt $ \(msg, usr, _chan, rct) -> do
    npEmoji <- P.asks @Config $ view #pointAssignEmoji
    guard $ npEmoji == rct
    awardMessagePoint msg usr

  void $ react @'MessageReactionRemoveEvt $ \(msg, usr, _chan, rct) -> do
    pointEmoji <- P.asks @Config $ view #pointAssignEmoji
    when (pointEmoji == rct) $ do
      db_ $ deleteWhere
        [ MessagePointMessage ==. (msg ^. #id)
        , MessagePointAssignedBy ==. (usr ^. #id)
        ]
      mmsg <- P.atomicGets @MessagePointMessages (view $ #messages . to (Map.lookup (msg ^. #id)))
      case mmsg of
        Nothing -> pure ()
        Just (awardMsg, 1) -> do
          void . invoke $ DeleteMessage awardMsg awardMsg
          P.atomicModify' @MessagePointMessages $ #messages %~ Map.delete (msg ^. #id)
        Just (awardMsg, amnt) -> do
          Just gid <- pure $ msg ^. #guildID
          points <- countPoints (msg ^. #author) gid
          void . invoke . EditMessage awardMsg awardMsg . editMessageContent . Just $
            awardPointMessageText msg (pred amnt) points ^. strict
          P.atomicModify' @MessagePointMessages $ #messages %~ Map.adjust (over _2 pred) (msg ^. #id)

  void $ react @'VoiceStateUpdateEvt $ \(mbefore, after') -> do
    roleList <- P.asks @Config . view $ #voiceConfig . #roles
    Just gid <- pure $ after' ^. #guildID
    for_ roleList $ \r -> if
      | (mbefore >>= view #channelID) `notElem` fmap Just (r ^. #voiceChannels)
      , (after' ^. #channelID) `elem` fmap Just (r ^. #voiceChannels) -> do
        void . invoke $ AddGuildMemberRole gid (after' ^. #userID) (r ^. #role)
        whenJust (r ^. #textChannel) $ \chanid -> do
          Just chan <- upgrade chanid
          Just usr <- upgrade (after' ^. #userID)
          void . invoke $ CreateMessage chan
            (def & #content ?~ "**" <> (usr ^. #username . strict) <> "** joined the channel")
      | (mbefore >>= view #channelID) `elem` fmap Just (r ^. #voiceChannels)
      , (after' ^. #channelID) `notElem` fmap Just (r ^. #voiceChannels) -> do
        void . invoke $ RemoveGuildMemberRole gid (after' ^. #userID) (r ^. #role)
        whenJust (r ^. #textChannel) $ \chanid -> do
          Just chan <- upgrade chanid
          Just usr <- upgrade (after' ^. #userID)
          void . invoke $ CreateMessage chan
            (def & #content ?~ "**" <> (usr ^. #username . strict) <> "** left the channel")
      | otherwise -> pure ()

awardMessagePoint ::
  ( BotC r
  , P.Members
    [ Persistable
    , P.NonDet
    , P.Fail
    , P.AtomicState MessagePointMessages
    , P.GhcTime
    ] r
  ) => Message -> User -> P.Sem r ()
awardMessagePoint msg usr = do
  Just gid <- pure (msg ^. #guildID)
  Right mem <- invoke $ GetGuildMember gid usr
  perms <- permissionsIn' gid mem
  guard $ perms `containsAll` administrator
  time <- P.now
  db_ $ insert (MessagePoint (msg ^.  #id) gid (usr ^. #id) (msg ^. #author) time)
  points <- countPoints (msg ^. #author) gid
  mawardMsg <- P.atomicGets @MessagePointMessages (view $ #messages . to (Map.lookup $ msg ^. #id))
  case mawardMsg of
    Nothing -> do
      Right awardMsg <- tellt msg $
          awardPointMessageText msg 1 points
      P.atomicModify' @MessagePointMessages $ #messages %~ Map.insert (msg ^. #id) (awardMsg, 1)
    Just (awardMsg, amnt) -> do
      void . invoke . EditMessage awardMsg awardMsg . editMessageContent . Just $
          awardPointMessageText msg (succ amnt) points ^. strict
      P.atomicModify' @MessagePointMessages $ #messages %~ Map.adjust (over _2 succ) (msg ^. #id)

awardPointMessageText :: Message -> Int -> Int -> L.Text
awardPointMessageText msg points total =
  msg ^. #author . to mention
  <> " has been awarded " <> showPoints points ^. lazy <> " for being an awesome panda!\nThey now have "
  <> showPoints total ^. lazy  <> " total."
