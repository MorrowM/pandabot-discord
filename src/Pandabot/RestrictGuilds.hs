module Pandabot.RestrictGuilds where

import           Calamity
import           Optics
import           Control.Monad
import qualified Polysemy            as P
import qualified Polysemy.Fail       as P
import qualified Polysemy.Reader     as P

import           Pandabot.Bot.Config
import           Pandabot.Bot.Util

registerLeaveUnallowedGuilds ::
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerLeaveUnallowedGuilds = void $ react @'GuildCreateEvt $ \(g, _gstatus) -> do
  mallowed <- P.asks @Config $ view #allowedGuilds
  whenJust mallowed $ \allowed ->
    when (getID g `notElem` allowed) $
      void . invoke $ LeaveGuild g
