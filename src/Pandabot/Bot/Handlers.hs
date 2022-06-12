module Pandabot.Bot.Handlers
( registerEventHandlers
) where

import Calamity
import Calamity.Commands as C
import Calamity.Commands.Context ( FullContext )
import Control.Monad
import Optics
import Polysemy qualified as P
import Polysemy.AtomicState qualified as P
import Polysemy.Fail qualified as P
import Polysemy.Reader qualified as P
import Polysemy.Time qualified as P
import TextShow

import Pandabot.Bot.Config
import Pandabot.Bot.Database
import Pandabot.Bot.Util
import Pandabot.Buttons
import Pandabot.Modtools
import Pandabot.PandaRole
import Pandabot.Points
import Pandabot.RestrictGuilds
import Pandabot.VoiceLog

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
    info $ "Command failed with reason: " <> showt e
    case e of
      ParseError n r -> void . tellt ctx $ "Failed to parse parameter: `" <> n <> "`, with reason: ```\n" <> r <> "```"
      CheckError n r -> void . tellt ctx $ "Failed to pass a check: `" <> n <> "`, with reason: ```\n" <> r <> "```"
      InvokeError n r -> void . tellt ctx $ "Failed to invoke command `" <> n <> "`, with reason: ```\n" <> r <> "```"
    void . invoke $ CreateReaction ctx ctx (UnicodeEmoji "❌")

  void $ react @('CustomEvt (CommandInvoked FullContext)) \(CommandInvoked ctx) -> do
    info $ "Command invoked by " <> ctx ^. #user % #username <> ":\n" <> ctx ^. #message % #content
    emoj <- P.asks @Config $ view #reactPositiveEmoji
    void . invoke $ CreateReaction ctx ctx emoj
