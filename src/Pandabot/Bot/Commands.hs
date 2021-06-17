{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Pandabot.Bot.Commands
  ( registerBotCommands
  ) where

import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context   (FullContext)
import           CalamityCommands            (ConstructContext, ParsePrefix)
import           Control.Monad
import qualified Polysemy                    as P
import qualified Polysemy.AtomicState        as P
import qualified Polysemy.Fail               as P
import qualified Polysemy.Time               as P

import           Control.Lens
import           Data.Text                   (Text)
import           Pandabot.Bot.Commands.Utils
import           Pandabot.Bot.Database
import           Pandabot.Buttons
import           Pandabot.Help
import           Pandabot.Modtools
import           Pandabot.Points

-- | Register all the bot commands
registerBotCommands ::
  ( BotC r
  , P.Members
    [ ParsePrefix Message
    , Persistable
    , P.Fail
    , P.GhcTime
    , ConstructContext Message FullContext IO ()
    , P.AtomicState LockdownState
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


  command @'[] "grain" $ \ctx -> do
    void . invoke $ CreateReaction (ctx ^. #channel) (ctx ^. #message) (UnicodeEmoji "❌")
    void $ reply @Text (ctx ^. #message) "Error: Command `grain` not found! Did you mean `grian`?"
