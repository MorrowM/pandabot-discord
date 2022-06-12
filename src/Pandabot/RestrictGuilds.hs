module Pandabot.RestrictGuilds where

import Calamity
import Control.Monad
import Optics
import Polysemy qualified as P
import Polysemy.Fail qualified as P
import Polysemy.Reader qualified as P

import Pandabot.Bot.Config
import Pandabot.Bot.Util

registerLeaveUnallowedGuilds ::
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerLeaveUnallowedGuilds = void $ react @'GuildCreateEvt \(g, _gstatus) -> do
  mallowed <- P.asks @Config $ view #allowedGuilds
  whenJust mallowed \allowed ->
    when (getID g `notElem` allowed) do
      void . invoke $ LeaveGuild g
