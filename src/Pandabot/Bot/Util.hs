module Pandabot.Bot.Util
  ( handleFailByLogging
  , info
  , debug
  , tellt
  , whenJust
  , invoke_
  , (.:)
  , tellt_
  , allVals
  , tell_
  ) where

import           Calamity
import           Control.Monad
import           Data.Text        (Text)
import           Data.Text.Optics
import qualified DiPolysemy       as DiP
import           Optics
import qualified Polysemy         as P
import qualified Polysemy.Fail    as P

-- | Handle the `Fail` effect by logging it.
handleFailByLogging :: BotC r => P.Sem (P.Fail : r) a -> P.Sem r ()
handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> DiP.error @Text (e ^. packed)
    _      -> pure ()

-- | Logging functions.
info, debug :: BotC r => Text -> P.Sem r ()
info = DiP.info
debug = DiP.info

-- | `tell` a `Text`.
tellt :: (BotC r, Tellable t) => t -> Text -> P.Sem r (Either RestError Message)
tellt = tell @Text

-- | `tell` a `Text`, discarding the result.
tellt_ :: (BotC r, Tellable t) => t -> Text -> P.Sem r ()
tellt_ = void .: tell @Text

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust = flip $ maybe (pure ())


invoke_ :: _ => a -> P.Sem r ()
invoke_ = void . invoke

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

allVals :: (Enum a, Bounded a) => [a]
allVals = [minBound..maxBound]

tell_ :: forall msg r t. (BotC r, ToMessage msg, Tellable t) => t -> msg -> P.Sem r ()
tell_ c msg = void $ tell c msg
