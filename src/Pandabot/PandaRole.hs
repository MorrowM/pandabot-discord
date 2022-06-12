module Pandabot.PandaRole
  ( registerPandaRoleGiveHandler
  ) where

import Calamity
import Control.Monad
import Optics
import Polysemy qualified as P
import Polysemy.AtomicState qualified as P
import Polysemy.Fail qualified as P
import Polysemy.Reader qualified as P
import TextShow

import Pandabot.Bot.Config
import Pandabot.Bot.Util
import Pandabot.Modtools

registerPandaRoleGiveHandler ::
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.AtomicState LockdownState
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerPandaRoleGiveHandler = void $ react @'GuildMemberAddEvt \(guild, mem) -> do
    info $ "User " <> (mem ^. #username) <> " joined guild " <> showt (getID @Guild guild)
    ldState <- P.atomicGet @LockdownState
    when (ldState == Unlocked) do
      wrole <- P.asks @Config $ view #welcomeRole
      void . invoke $ AddGuildMemberRole guild mem wrole
