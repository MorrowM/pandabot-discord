module Pandabot.Bot.Util
  ( handleFailByLogging
  , info
  , debug
  , tellt
  , whenJust
  , tellembed
  , tellts
  , invoke_
  , (.:)
  , tellts_
  , tellt_
  , allVals
  ) where

import           Calamity
import           Control.Lens
import           Control.Monad
import           Data.Default
import qualified Data.Text      as S
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import           Data.Text.Lens
import qualified DiPolysemy     as DiP
import qualified Polysemy       as P
import qualified Polysemy.Fail  as P

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

-- | `tell` a lazy `Text`.
tellt :: (BotC r, Tellable t) => t -> Text -> P.Sem r (Either RestError Message)
tellt t m = tell t $ L.toStrict m

-- | `tell` a lazy `Text`, discarding the result.
tellt_ :: (BotC r, Tellable t) => t -> Text -> P.Sem r ()
tellt_ t m = void $ tell t $ L.toStrict m

-- | `tell` a strict `Text`.
tellts :: (BotC r, Tellable t) => t -> S.Text -> P.Sem r (Either RestError Message)
tellts = tell @S.Text

-- | `tell` a strict `Text`, discarding the result.
tellts_ :: (BotC r, Tellable t) => t -> S.Text -> P.Sem r ()
tellts_ = void .: tell @S.Text

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust = flip $ maybe (pure ())

tellembed :: (BotC r, HasID Channel c) => c -> Embed -> P.Sem r ()
tellembed ctx emb = void $ invoke $ CreateMessage (getID @Channel ctx) $ def & #embed ?~ emb

invoke_ :: _ => a -> P.Sem r ()
invoke_ = void . invoke

(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)

allVals :: (Enum a, Bounded a) => [a]
allVals = [minBound..maxBound]
