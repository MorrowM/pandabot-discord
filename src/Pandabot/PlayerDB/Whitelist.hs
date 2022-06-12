{-# LANGUAGE DuplicateRecordFields #-}
module Pandabot.PlayerDB.Whitelist where

import Pandabot.Bot.Schema

import Data.Aeson
import Data.Aeson.Optics
import Data.Proxy
import Data.Text ( Text )
import Data.Traversable
import GHC.Generics ( Generic )
import Network.HTTP.Req hiding ( Req, req )
import Network.HTTP.Req qualified as R ( req )
import Optics
import Optics.Operators.Unsafe ( (^?!) )
import Polysemy qualified as P

data Req m a where
  Req ::
    (
      HttpMethod method,
      HttpBody body,
      HttpResponse a,
      HttpBodyAllowed (AllowsBody method) (ProvidesBody body)
    ) =>  method ->  Url scheme ->  body -> Proxy a ->  Option scheme -> Req m a

P.makeSem ''Req

runReqInIO :: P.Member (P.Embed IO) r => P.InterpreterFor Req r
runReqInIO = P.interpret $ \case
  Req m s b p o  -> P.embed @IO $ runReq defaultHttpConfig (R.req m s b p o)

data WhitelistEntry = WhitelistEntry
  { uuid :: UUID
  , name :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Whitelist = Whitelist
  { getWhitelist :: [WhitelistEntry]
  }
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

data NameHistoryEntry = NameHistoryEntry
  { name        :: Text
  , changedToAt :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype NameHistory = NameHistory [NameHistoryEntry]
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

fetchEntryByName :: P.Member Req r => Text -> P.Sem r Value
fetchEntryByName nm =
  responseBody <$> req GET (https "api.mojang.com" /: "users" /: "profiles" /: "minecraft" /: nm)
                       NoReqBody jsonResponse mempty

fetchNameHistoryByUUID :: P.Member Req r => UUID -> P.Sem r NameHistory
fetchNameHistoryByUUID nm =
  responseBody <$> req GET (https "api.mojang.com" /: "user" /: "profiles" /: getUUID nm /: "names")
                       NoReqBody jsonResponse mempty

fetchUUIDByName :: P.Member Req r => Text -> P.Sem r UUID
fetchUUIDByName name =  do
  entry <- fetchEntryByName name
  pure $ UUID $ entry ^?! key "id" % _String

fetchCurrentNameByUUID :: P.Member Req r => UUID -> P.Sem r Text
fetchCurrentNameByUUID u = do
  NameHistory nh <- fetchNameHistoryByUUID u
  pure $ nh ^. to last % #name

fetchWhitelist :: P.Member Req r => [UUID] -> P.Sem r Whitelist
fetchWhitelist uuids = Whitelist <$> do
  for uuids \u -> do
    nm <- fetchCurrentNameByUUID u
    pure $ WhitelistEntry u nm
