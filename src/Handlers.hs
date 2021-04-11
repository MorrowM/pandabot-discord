module Handlers where

import           Calamity
import           Calamity.Cache.Eff
import           Calamity.Commands
import qualified Calamity.Commands    as CommandContext
import           Control.Lens
import           Control.Monad
import           Data.Foldable
import qualified Data.Text.Lazy       as L
import qualified Data.Vector.Unboxing as V
import           Database.Persist
import qualified Polysemy             as P
import qualified Polysemy.Fail        as P
import qualified Polysemy.NonDet      as P
import           TextShow

import           Database
import           Schema
import           Util

registerEventHandlers :: (BotC r, P.Member Persistable r, P.Member P.Fail r) => P.Sem r ()
registerEventHandlers = do
  void $ react @('CustomEvt "command-error" (CommandContext.Context, CommandError)) $ \(ctx, e) -> do
        info $ "Command failed with reason: " <> showtl e
        case e of
          ParseError n r -> void . tellt ctx $ "Failed to parse parameter: `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
          _ -> pure ()
  void $ P.runNonDetMaybe $ react @'RawMessageReactionAddEvt $ \rinfo -> do
    Just gid <- pure (rinfo ^. #guildID)
    Just myUid <- fmap getID <$> getBotUser
    guard $ myUid /= (rinfo ^. #userID)
    btns <- fmap (fmap entityVal) $ db $ selectList [ButtonMessage ==. (rinfo ^. #messageID), ButtonEmoji ==. showt (rinfo ^. #emoji)] []
    for_ btns $ \btn -> do
      Right mem <- invoke $ GetGuildMember gid (rinfo ^. #userID)
      void $ if buttonRole btn `V.elem` (mem ^. #roles)
        then invoke $ RemoveGuildMemberRole gid (mem ^. #id) (buttonRole btn)
        else invoke $ AddGuildMemberRole    gid (mem ^. #id) (buttonRole btn)
      invoke $ DeleteUserReaction (rinfo ^. #channelID) (rinfo ^. #messageID) (rinfo ^. #emoji) mem

