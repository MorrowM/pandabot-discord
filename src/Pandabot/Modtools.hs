module Pandabot.Modtools
  ( registerLockdownCommand
  , LockdownState (..)
  , toggleLockdown
  , toggleLockdownPure
  ) where

import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           Control.Lens
import           Control.Monad
import           Data.Text                 (Text)
import           GHC.Generics
import qualified Polysemy                  as P
import qualified Polysemy.AtomicState      as P
import           TextShow
import           TextShow.Generic

data LockdownState = Locked | Unlocked
  deriving stock (Show, Eq, Enum, Bounded, Ord, Generic)
  deriving TextShow via (FromGeneric LockdownState)

registerLockdownCommand ::
  ( BotC r
  , P.Member (P.AtomicState LockdownState) r
  ) => Check FullContext -> P.Sem (DSLState FullContext r) ()
registerLockdownCommand admin = void
  $ requires [admin]
  $ help (const "Temporarily disables members from getting the @Pandas role.")
  $ hide
  $ command @'[] "lockdown" $ \ctx -> do
    ldState <- toggleLockdown
    void $ reply @Text (ctx ^. #message) $ "Lockdown state: " <> showt ldState

toggleLockdownPure :: LockdownState -> LockdownState
toggleLockdownPure Locked   = Unlocked
toggleLockdownPure Unlocked = Locked

toggleLockdown :: P.Member (P.AtomicState LockdownState) r => P.Sem r LockdownState
toggleLockdown = do
  P.atomicModify' toggleLockdownPure
  P.atomicGet
