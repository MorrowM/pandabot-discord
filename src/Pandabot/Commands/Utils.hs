module Pandabot.Commands.Utils where

import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           CalamityCommands.Check    (buildCheck)
import           Control.Lens
import           Control.Monad
import           Data.Flags
import qualified Polysemy                  as P
import qualified Polysemy.Fail             as P

-- | Create a `Check` for whether the user invoking the
-- command is an administrator.
isAdmin :: BotC r => P.Sem r (Check FullContext)
isAdmin = buildCheck "requires admin" $ \ctx -> do
  case ctx ^. #member of
    Nothing -> pure $ Just "This command can only be run in a server"
    Just mem -> do
      perms <- permissionsIn' (mem ^. #guildID) mem
      pure $ if perms `containsAll` administrator
        then Nothing
        else Just "User must be an administrator"

sameGuild :: (BotC r, P.Member P.Fail r) => FullContext -> GuildChannel -> P.Sem r ()
sameGuild ctx chan = when (Just (getID @Guild chan) == (getID <$> ctx ^. #guild)) $ do
        fire $ customEvt (CtxCommandError ctx $ CheckError "same guild" "Cannot modify buttons in other guilds")
