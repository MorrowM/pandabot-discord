module Pandabot.Modtools
  ( registerLockdownCommand
  , LockdownState (..)
  , toggleLockdown
  , toggleLockdownPure
  ) where

import           Calamity
import           Calamity.Commands
import           Control.Monad
import qualified Polysemy             as P
import qualified Polysemy.AtomicState as P

data LockdownState = Locked | Unlocked
  deriving (Show, Eq, Enum, Bounded, Ord)

registerLockdownCommand ::
  ( BotC r
  , P.Member (P.AtomicState LockdownState) r
  , CommandContext c
  ) => Check c -> P.Sem (DSLState c r) ()
registerLockdownCommand admin = void
  $ requires [admin]
  $ help (const "Server lockdown")
  $ command @'[] "lockdown" $ \_ctx -> do
    toggleLockdown

toggleLockdownPure :: LockdownState -> LockdownState
toggleLockdownPure Locked   = Unlocked
toggleLockdownPure Unlocked = Locked

toggleLockdown :: P.Member (P.AtomicState LockdownState) r => P.Sem r ()
toggleLockdown = P.atomicModify' toggleLockdownPure
