module Pandabot.Bot.Handlers
( registerEventHandlers
) where

import           Calamity
import           Calamity.Commands         as C
import           Calamity.Commands.Context (FullContext)
import           Control.Lens
import           Control.Monad
import qualified Data.Text.Lazy            as L
import qualified Polysemy                  as P
import qualified Polysemy.AtomicState      as P
import qualified Polysemy.Fail             as P
import qualified Polysemy.Reader           as P
import qualified Polysemy.Time             as P
import           TextShow

import           Pandabot.Bot.Config
import           Pandabot.Bot.Database
import           Pandabot.Bot.Util
import           Pandabot.Buttons
import           Pandabot.Modtools
import           Pandabot.PandaRole
import           Pandabot.Points
import           Pandabot.RestrictGuilds
import           Pandabot.VoiceLog

-- | Register the various event handlers for the bot.
registerEventHandlers ::
  ( BotC r
  , P.Members
    [ Persistable
    , P.Fail
    , P.Reader Config
    , P.AtomicState MessagePointMessages
    , P.GhcTime
    , P.AtomicState LockdownState
    ] r )
  => P.Sem r ()
registerEventHandlers = do
  registerCommandResponseHandler
  registerVoiceLogHandler
  registerButtonPressHandler
  registerPandaRoleGiveHandler
  registerPointGiveHandler
  registerPointRevokeHandler
  registerLeaveUnallowedGuilds

registerCommandResponseHandler ::
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.AtomicState MessagePointMessages
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerCommandResponseHandler = do
  void $ react @('CustomEvt (CtxCommandError FullContext)) $ \(CtxCommandError ctx e) -> do
    info $ "Command failed with reason: " <> showtl e
    case e of
      ParseError n r -> void . tellt ctx $ "Failed to parse parameter: `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
      CheckError n r -> void . tellt ctx $ "Failed to pass a check: `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
      InvokeError n r -> void . tellt ctx $ "Failed to invoke command `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
    let msg = ctx ^. #message
    void . invoke $ CreateReaction msg msg (UnicodeEmoji "❌")

  void $ react @('CustomEvt (CommandInvoked FullContext)) $ \(CommandInvoked ctx) -> do
    info $ "Command invoked by " <> ctx ^. #user . #username <> ":\n" <> ctx ^. #message . #content
    emoj <- P.asks @Config $ view #reactPositiveEmoji
    let msg = ctx ^. #message
    void . invoke $ CreateReaction msg msg emoj
