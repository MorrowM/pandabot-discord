module Pandabot.PinTool
  ( registerPinToolHandler
  ) where

import           Calamity
import           Control.Monad
import qualified Data.Vector.Unboxing as V
import           Optics
import           Pandabot.Bot.Config
import           Pandabot.Bot.Util
import qualified Polysemy             as P
import qualified Polysemy.Fail        as P
import qualified Polysemy.NonDet      as P
import qualified Polysemy.Reader      as P

registerPinToolHandler ::
  ( BotC r
  , P.Members
      [ P.Fail
      , P.Reader Config
      ]
      r
  ) =>
  P.Sem r ()
registerPinToolHandler = void $
  react @ 'MessageReactionAddEvt $ \(msg, user, chan, rawEmoji) -> void $
    P.runNonDetMaybe $ do
      GuildChannel' gc <- pure chan
      Right mem <- invoke $ GetGuildMember gc user

      cfg <- P.ask @Config
      let pinEmoji = cfg ^. #pinConfig % #emoji
          pinRoles = cfg ^. #pinConfig % #roles

      guard $ V.any (`elem` pinRoles) (mem ^. #roles)
      guard $ rawEmoji == pinEmoji

      void $ if msg ^. #pinned
        then invoke $ DeletePinnedMessage chan msg
        else invoke $ AddPinnedMessage chan msg

      invoke_ $ DeleteUserReaction chan msg rawEmoji user
