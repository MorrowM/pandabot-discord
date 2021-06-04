module Pandabot.PandaRole
  ( registerPandaRoleGiveHandler
  ) where

import           Calamity
import           Control.Lens
import           Control.Monad
import qualified Polysemy             as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Fail        as P
import qualified Polysemy.Reader      as P
import           TextShow

import           Pandabot.Bot.Config
import           Pandabot.Bot.Util
import           Pandabot.Modtools

registerPandaRoleGiveHandler ::
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.AtomicState LockdownState
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerPandaRoleGiveHandler = void $ react @'GuildMemberAddEvt $ \mem -> do
    info $ "User " <> (mem ^. #username) <> " joined guild " <> (mem ^. #guildID . to showt . lazy)
    ldState <- P.atomicGet @LockdownState
    when (ldState == Unlocked) $ do
      wrole <- P.asks @Config $ view #welcomeRole
      void . invoke $ AddGuildMemberRole (mem ^. #guildID) (mem ^. #id) wrole
