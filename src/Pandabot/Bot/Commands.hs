{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Pandabot.Bot.Commands
  ( registerBotCommands
  ) where

import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context   (FullContext)
import           CalamityCommands            (ConstructContext, ParsePrefix)
import           Control.Monad
import           Data.Text                   (Text)
import qualified Polysemy                    as P
import qualified Polysemy.AtomicState        as P
import qualified Polysemy.Fail               as P
import qualified Polysemy.Time               as P


import           Pandabot.Bot.Commands.Utils
import           Pandabot.Bot.Database
import           Pandabot.Buttons
import           Pandabot.Help
import           Pandabot.Modtools
import           Pandabot.PlayerDB
import           Pandabot.PlayerDB.Whitelist
import           Pandabot.Points

-- | Register all the bot commands
registerBotCommands ::
  ( BotC r
  , P.Members
    [ ParsePrefix Message
    , Persistable
    , P.Fail
    , P.GhcTime
    , ConstructContext (Message, User, Maybe Member) FullContext IO ()
    , P.AtomicState LockdownState
    , Req
    ] r
  ) => P.Sem r ()
registerBotCommands = void $ addCommands $ do
  admin <- isAdmin
  void customHelpCommand
  void $ requires [admin] adminHelpCommand

  registerButtonCommands admin

  registerAwardCommand admin
  registerLeaderboardCommand
  registerShootsCommand

  registerLockdownCommand admin

  registerPlayerCommands admin

  hide
    $ help (const "<:Wheat:857132853249966081>")
    $ command @'[] "grain" $ \ctx -> do
    void . invoke $ CreateReaction ctx ctx (UnicodeEmoji "❌")
    void $ reply @Text ctx "Error: Command `grain` not found! Did you mean `grian`?"
