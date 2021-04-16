module Pandabot.Util
  ( handleFailByLogging
  , info
  , debug
  , tellt
  , showPoints
  , whenJust
  ) where

import           Calamity
import           Control.Lens
import qualified Data.Text      as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import           Data.Text.Lens
import qualified Di
import qualified DiPolysemy     as DiP
import qualified Polysemy       as P
import qualified Polysemy.Fail  as P
import           TextShow

-- | Handle the `Fail` effect by logging it.
handleFailByLogging :: BotC r => P.Sem (P.Fail : r) a -> P.Sem r ()
handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> DiP.error @Text @Di.Path (e ^. packed)
    _      -> pure ()

-- | Logging functions.
info, debug :: BotC r => Text -> P.Sem r ()
info = DiP.info @Text @Di.Path
debug = DiP.info @Text @Di.Path

-- | `tell` a lazy `Text`.
tellt :: (BotC r, Tellable t) => t -> Text -> P.Sem r (Either RestError Message)
tellt t m = tell t $ L.toStrict m

-- | Format a point value to a phrase with proper grammer.
showPoints :: Int -> T.Text
showPoints 0 = "no bamboo shoots"
showPoints 1 = "1 bamboo shoot"
showPoints n = showt n <> " bamboo shoots"

whenJust :: Applicative f => Maybe a -> (a -> f ()) -> f ()
whenJust = flip $ maybe (pure ())
