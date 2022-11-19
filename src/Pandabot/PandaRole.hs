module Pandabot.PandaRole
  ( registerPandaRoleGiveHandler
  ) where

import           Calamity
import           Control.Monad
import           Optics
import qualified Polysemy             as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Fail        as P
import qualified Polysemy.Reader      as P

import           Pandabot.Bot.Config
import           Pandabot.Bot.Util
import           Pandabot.Modtools
import           TextShow

registerPandaRoleGiveHandler ::
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.AtomicState LockdownState
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerPandaRoleGiveHandler = void $ react @'GuildMemberAddEvt $ \(g, mem) -> do
    info $ "User " <> (mem ^. #username) <> " joined guild " <> (g ^. #id % to showt)
    ldState <- P.atomicGet @LockdownState
    when (ldState == Unlocked) $ do
      wrole <- P.asks @Config $ view #welcomeRole
      void . invoke $ AddGuildMemberRole (Snowflake @Guild 753002885633146932) mem wrole
