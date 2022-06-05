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

registerPandaRoleGiveHandler ::
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.AtomicState LockdownState
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerPandaRoleGiveHandler = void $ react @'GuildMemberAddEvt $ \mem -> do
    -- TODO Fix this as well when the guild ID bug is fixed. See the TODO lower down.
    info $ "User " <> (mem ^. #username) <> " joined guild " -- <> (mem ^. #guildID % to showt)
    ldState <- P.atomicGet @LockdownState
    when (ldState == Unlocked) $ do
      wrole <- P.asks @Config $ view #welcomeRole
      void . invoke $ AddGuildMemberRole (Snowflake @Guild 753002885633146932) mem wrole
      -- TODO The guild ID is hardcoded because of a bug in calamity.
      --      The GuildMemberAddEvt event does not include the guild ID in its
      --      payload even though the changelog says it should.
