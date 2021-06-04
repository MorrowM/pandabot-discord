module Pandabot.Bot.Util
  ( handleFailByLogging
  , info
  , debug
  , tellt
  , whenJust
  ) where

import           Calamity
import           Control.Lens
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

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust = flip $ maybe (pure ())
