module Util where

import           Calamity
import           Control.Lens
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as L
import           Data.Text.Lens
import qualified Di
import qualified DiPolysemy     as DiP
import qualified Polysemy       as P
import qualified Polysemy.Fail  as P


handleFailByLogging :: BotC r => P.Sem (P.Fail : r) a -> P.Sem r ()
handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> DiP.error @Text @Di.Path (e ^. packed)
    _      -> pure ()

info, debug :: BotC r => Text -> P.Sem r ()
info = DiP.info @Text @Di.Path
debug = DiP.info @Text @Di.Path

tellt :: (BotC r, Tellable t) => t -> Text -> P.Sem r (Either RestError Message)
tellt t m = tell t $ L.toStrict m
