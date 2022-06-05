module Pandabot.Bot.Commands.Utils where

import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           CalamityCommands.Check    (buildCheck)
import           Optics
import           Control.Monad
import           Data.Flags
import qualified Polysemy                  as P
import qualified Polysemy.Fail             as P

-- | Create a `Check` for whether the user invoking the
-- command is an administrator.
isAdmin :: BotC r => P.Sem r (Check FullContext)
isAdmin = buildCheck "Requires Admin" $ \ctx -> do
  case (ctx ^. #guild, ctx ^. #member) of
    (Just guild, Just mem) -> do
      perms <- permissionsIn' guild mem
      pure $ if perms `containsAll` administrator
        then Nothing
        else Just "User must be an administrator."
    _ -> pure $ Just "This command can only be run in a server."

sameGuild :: (BotC r, P.Member P.Fail r) => FullContext -> GuildChannel -> P.Sem r ()
sameGuild ctx chan = when (Just (getID @Guild chan) == (getID <$> ctx ^. #guild)) $ do
        fire $ customEvt (CtxCommandError ctx $ CheckError "same guild" "Cannot modify buttons in other guilds")
        fail "Cannot modify buttons in other guilds"
