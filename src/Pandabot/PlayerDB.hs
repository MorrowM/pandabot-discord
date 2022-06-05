{-# LANGUAGE RecordWildCards #-}
module Pandabot.PlayerDB where


import           Pandabot.Bot.Database
import           Pandabot.Bot.Schema
import           Pandabot.Bot.Util

import           Calamity                        hiding (select)
import           Calamity.Commands
import           Calamity.Commands.Context       (FullContext)
import           Control.Monad
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy            (ByteString)
import           Data.Default
import           Data.Int
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time
import           Data.Time.Format.ISO8601
import           Data.Traversable
import           Database.Esqueleto.Experimental (BackendKey (unSqlBackendKey),
                                                  Entity (Entity), from,
                                                  innerJoin, insert, like,
                                                  select, table,
                                                  type (:&) ((:&)), val, where_,
                                                  (++.), (==.))
import qualified Database.Esqueleto.Experimental as E
import qualified Database.Persist                as P
import           GHC.Generics                    (Generic)
import           Optics
import           Pandabot.PlayerDB.Whitelist
import qualified Polysemy                        as P
import           Polysemy.Time
import           TextShow


createEmptyCommunityMember :: P.Members '[Persistable, GhcTime] r
  => P.Sem r (Key CommunityMember)
createEmptyCommunityMember = do
  createdAt <- now
  db $ insert $ CommunityMember StandardMember createdAt

createCommunityMember :: P.Members '[Persistable, GhcTime, Req] r
  => NameType -> Text -> P.Sem r (Key MemberName, Key CommunityMember)
createCommunityMember nametype name = do
  memid <- createEmptyCommunityMember
  uuid <- case nametype of
    MinecraftJavaName -> Just <$>  fetchUUIDByName name
    _                 -> pure Nothing
  fmap (,memid) $ db $ insert (MemberName memid nametype name uuid)

linkMemberNames :: P.Members '[Persistable, GhcTime, Req] r
  => NameType -> Text -> NameType -> Text -> P.Sem r (Maybe (Key CommunityMember))
linkMemberNames nametype name nametype' name' = do
  mexisting <- db $ P.getBy $ UniqueNameTypeName nametype name
  memid <- case mexisting of
        Nothing       -> do
          snd <$> createCommunityMember nametype name
        Just (Entity _ (MemberName memid _ _ _)) -> pure memid
  uuid <- case nametype' of
    MinecraftJavaName -> Just <$>  fetchUUIDByName name'
    _                 -> pure Nothing
  res <- db $ P.insertUnique (MemberName memid nametype' name' uuid)
  pure $ memid <$ res

searchCommunityMember :: P.Member Persistable r
  => NameType -> Text -> P.Sem r [(Entity CommunityMember, [MemberName])]
searchCommunityMember nametype name = do
  res <- db $ select $ do
    memname <- from $ table @MemberName
    where_ (memname E.^. MemberNameName `like` (E.%) ++. val name ++. (E.%))
    where_ (memname E.^. MemberNameNameType ==. val nametype)
    pure memname

  for res $ \(Entity _ (MemberName memid _ _ _)) -> db $ do
    searchres <- map P.entityVal <$> P.selectList [MemberNameMemberID P.==. memid] [P.Asc MemberNameNameType]
    Just mem <- P.get memid
    pure (Entity memid mem, searchres)

searchCommunityMemberAllTypes :: P.Member Persistable r
  => Text -> P.Sem r [MemberName]
searchCommunityMemberAllTypes name =
  fmap (map P.entityVal) $ db $ select $ do
    memname <- from $ table @MemberName
    where_ (memname E.^. MemberNameName `like` (E.%) ++. val name ++. (E.%))
    pure memname

getCommunityMemberById :: P.Member Persistable r
  => CommunityMemberId -> P.Sem r (Maybe (Entity CommunityMember, [MemberName]))
getCommunityMemberById memid = do
  mmem <- db $ P.get memid
  case mmem of
    Nothing -> pure Nothing
    Just mem -> do
      names <- db $ P.selectList [MemberNameMemberID P.==. memid] [P.Asc MemberNameNameType]
      pure $ Just (Entity memid mem, P.entityVal <$> names)

removeCommunityMemberName :: P.Member Persistable r
  => NameType -> Text -> P.Sem r (Maybe (Entity CommunityMember, [MemberName]))
removeCommunityMemberName nametype name = do
  mexisting <- db $ P.getBy $ UniqueNameTypeName nametype name
  case mexisting of
    Nothing                                -> pure Nothing
    Just (Entity _ (MemberName memid _ _ _)) -> do
      db $ P.deleteBy $ UniqueNameTypeName nametype name
      getCommunityMemberById memid

removeCommunityMember :: P.Member Persistable r
  => Key CommunityMember -> P.Sem r Bool
removeCommunityMember memid = do
  mmem <- db $ P.get memid
  case mmem of
    Nothing -> pure False
    Just _  -> True <$ db (P.delete memid)

data Summary = Summary
  { memberCountTotal      :: Int
  , memberCountByStatus   :: Map CommunityMemberStatus Int
  , memberNameCountByType :: Map NameType Int
  } deriving (Eq, Show, Generic)

getCommunityMemberSummary :: P.Member Persistable r
  => P.Sem r Summary
getCommunityMemberSummary = do
  total <- db $ P.count @_ @_ @CommunityMember []
  byStatus <- fmap Map.fromList $ for allVals $ \status -> do
    count <- db $ P.count [CommunityMemberStatus P.==. status]
    pure (status, count)
  byType <- fmap Map.fromList $ for allVals $ \ty -> do
    count <- db $ P.count [MemberNameNameType P.==. ty]
    pure (ty, count)
  pure $ Summary total byStatus byType

setCommunityMemberStatus :: P.Member Persistable r
  => NameType -> Text -> CommunityMemberStatus -> P.Sem r (Maybe CommunityMemberId)
setCommunityMemberStatus nametype name status = do
  mmemid <- db $ P.getBy $ UniqueNameTypeName nametype name
  case mmemid of
    Nothing -> pure Nothing
    Just (Entity _ (MemberName memid _ _ _))  -> do
      db $ P.update memid [CommunityMemberStatus P.=. status]
      pure $ Just memid

addMemberNameById :: P.Members '[Persistable, Req] r
  => CommunityMemberId -> NameType -> Text -> P.Sem r (Maybe MemberNameId)
addMemberNameById memid nametype name = do
  mmem <- db $ P.get memid
  uuid <- case nametype of
    MinecraftJavaName -> Just <$>  fetchUUIDByName name
    _                 -> pure Nothing
  case mmem of
    Nothing -> pure Nothing
    Just _  -> fmap Just $ db $ insert $ MemberName memid nametype name uuid

updateMemberName :: P.Members '[Persistable, Req] r
  => NameType -> Text -> Text -> P.Sem r (Maybe (Key CommunityMember))
updateMemberName nametype name name' = do
  mexisting <- db $ P.getBy $ UniqueNameTypeName nametype name
  case mexisting of
    Nothing -> pure Nothing
    Just (Entity nameid (MemberName memid _ _ _)) -> do
      uuid <- case nametype of
        MinecraftJavaName -> Just <$>  fetchUUIDByName name'
        _                 -> pure Nothing
      db $ P.update nameid [MemberNameName P.=. name', MemberNameUuid P.=. uuid]
      pure $ Just memid

generateWhitelist :: P.Members '[Persistable, Req] r
  => P.Sem r ByteString
generateWhitelist = do
  mns <- db $ select $ do
    (cms :& mns) <-
      from $ table @CommunityMember
      `innerJoin` table @MemberName
      `E.on` (\(cm :& mn) ->
              cm E.^. CommunityMemberId ==. mn E.^. MemberNameMemberID)
    where_ (cms E.^. CommunityMemberStatus  E.==. val Whitelisted)
    where_ (mns E.^. MemberNameNameType  E.==. val MinecraftJavaName)
    pure mns
  let uuids = [uuid | (Entity _ (MemberName _ _ _ (Just uuid))) <- mns]
  encodePretty . (#getWhitelist % mapped % #uuid %~ fmtUUID)  <$> fetchWhitelist uuids


registerPlayerCommands ::
  ( BotC r
  , P.Members '[Persistable, GhcTime, Req] r
  , DSLC FullContext r
  ) => Check FullContext -> P.Sem r ()
registerPlayerCommands admin
  = requires [admin]
  $ hide
  $ help (const "Query information about community members.")
  $ group "player"
  $ do
    -- Manage Community Members
    void
      $ help (const "Create a blank player entry.")
      $ command @'[NameType, Named "name" Text] "new" $ \ctx ty name -> do
      (_, memid) <- createCommunityMember ty name
      Just mem <- getCommunityMemberById memid
      invoke_ $ CreateMessage ctx $ def @CreateMessageOptions
        & #embeds ?~ [uncurry ppMemberGetResult mem]
        & #content ?~ "Success, created player with id " <>  showt (memid2int memid) <> "."

    void
      $ help (const "Display all info for a player.")
      $ command @'[Named "id" Int] "get" $ \ctx memid -> do
      mmem <- getCommunityMemberById (int2memid memid)
      case mmem of
        Nothing  -> tellt_ ctx "Error: Could not find player with that id."
        Just mem -> tell_ ctx (uncurry ppMemberGetResult mem)

    void
      $ help (const "Remove a player by id.")
      $ command @'[Named "id" Int] "remove" $ \ctx memid -> do
      res <- removeCommunityMember (int2memid memid)
      if res
        then tellt_ ctx "Success, player removed."
        else tellt_ ctx "Error: Player not found."

    -- Manage Member names
    void
      $ help (const "Add a player name given another name of that player. If that name does not exist, a new player will be created with both names.")
      $ command @'[NameType, Named "name" Text, NameType, Named "name" Text] "linkname" $ \ctx ty name ty' name' -> do
      mmemid <- linkMemberNames ty name ty' name'
      case mmemid of
        Nothing -> tellt_ ctx "Error: The given name and type already exist."
        Just memid -> do
          Just mem <- getCommunityMemberById memid
          tell_ @String ctx $ "Success, updated player with id " <>  show (memid2int memid) <> "."
          tell_ ctx $ uncurry ppMemberGetResult mem

    void
      $ help (const "Add a player name by id.")
      $ command @'[Named "id" Int, NameType, Named "name" Text] "addname" $ \ctx (int2memid -> memid) ty name -> do
      mmemid <- addMemberNameById memid ty name
      case mmemid of
        Nothing -> tellt_ ctx "Error: Player not found."
        Just _ -> do
          Just mem <- getCommunityMemberById memid
          tell_ @String ctx $ "Success, updated player with id " <>  show (memid2int memid) <> "."
          tell_ ctx $ uncurry ppMemberGetResult mem

    void
      $ help (const "Update a given name with another name.")
      $ command @'[NameType, Named "name" Text, Named "name" Text] "updatename" $ \ctx ty name name' -> do
      mmemid <- updateMemberName ty name name'
      case mmemid of
        Nothing -> tellt_ ctx "Error: Name not found."
        Just memid -> do
          Just mem <- getCommunityMemberById memid
          tell_ @String ctx $ "Success, updated player with id " <>  show (memid2int memid) <> "."
          tell_ ctx $ uncurry ppMemberGetResult mem

    -- Misc.
    void
      $ help (const "Search for a player by name.")
      $ command @'[Maybe NameType, Named "name" Text] "search" $ \ctx mty name -> do
      case mty of
        Nothing -> do
          res <- searchCommunityMemberAllTypes name
          case res of
            []    -> tellt_ ctx "Error: Could not find player with that name."
            names -> tell_ ctx $ ppGeneralSearchResults names
        Just ty -> do
          res <- searchCommunityMember ty name
          case res of
            [] -> tellt_ ctx "Error: Could not find player with that name and type."
            xs -> tell_ ctx $ ppMemberSearchResult xs
    void
      $ help (const "Remove a name entry for a player.")
      $ command @'[NameType, Named "name" Text] "removename" $ \ctx nametype name -> do
      mmem <- removeCommunityMemberName nametype name
      case mmem of
        Nothing -> tellt_ ctx "Error: Could not find player with that name and type."
        Just mem -> tell_ ctx (uncurry ppMemberGetResult mem)

    void
      $ help (const "Get a summary of all players.")
      $ command @'[] "summary" $ \ctx -> do
      Summary{..} <- getCommunityMemberSummary
      tell_ @Embed ctx ( def
        & #title ?~ "Player Summary"
        & #fields .~
              embedField "Total Players" (showt memberCountTotal)
              :  [EmbedField (ppType ty <> " Count") (showt cnt) True       | (ty,cnt)     <- Map.toAscList memberNameCountByType]
              <> [EmbedField (ppStatus status <> " Count") (showt cnt) True | (status,cnt) <- Map.toAscList memberCountByStatus]
        )

    void
      $ help (const "Set the status of a player.")
      $ command @'[NameType, Named "name" Text, CommunityMemberStatus] "setstatus" $ \ctx nametype name status -> do
      mmemid <- setCommunityMemberStatus nametype name status
      case mmemid of
        Nothing -> tellt_ ctx "Error: Could not find player with that name and type."
        Just memid -> do
          Just mem <- getCommunityMemberById memid
          invoke_ $ CreateMessage ctx (def
            & #content ?~ ("Success, updated player status." :: Text)
            & #embeds ?~ [uncurry ppMemberGetResult mem]
            )

    void
      $ help (const "Generate a whitelist.")
      $ command @'[] "whitelist" $ \ctx -> do
        wl <- generateWhitelist
        invoke_ $ CreateMessage ctx $ def
          & #attachments ?~ [CreateMessageAttachment "whitelist.json" (Just "Minecraft Whitelist file") wl]

ppMemberGetResult :: Entity CommunityMember -> [MemberName] -> Embed
ppMemberGetResult (Entity memid (CommunityMember status createdAt)) ty = def
      & #title ?~ "Member Lookup Result • Id " <> showt (memid2int memid)
      & #footer ?~ footer
      & #fields .~ fields
      & #timestamp ?~ createdAt
  where
    mkField (k, v) = EmbedField k v True
    fields
      = mkField . ppMemberNamePair <$> ty
    footer
      =  embedFooter (ppStatus status)

ppMemberSearchResult :: [(Entity CommunityMember, [MemberName])] -> Embed
ppMemberSearchResult mems = def
  & #title ?~ "Search Results"
  & #fields .~ flds
  & #footer ?~ embedFooter (fmtResultCount (length mems))
  where
    flds = concat
      [ EmbedField "Player Id" (showt memid) False
      : EmbedField "Status" (ppStatus status) True
      : EmbedField "Added" (ppCreatedAt createdAt) True
      : nameFields
      | (Entity (memid2int -> memid) (CommunityMember status createdAt), names) <- take 5 mems
      , let nameFields = map (\nm -> let (k, v) = ppMemberNamePair nm in EmbedField k v True) names
      ]

fmtResultCount :: Int -> T.Text
fmtResultCount 0 = "No Results"
fmtResultCount 1 = "1 Result"
fmtResultCount n = showt n <> " Results"


ppGeneralSearchResults :: [MemberName] -> Embed
ppGeneralSearchResults names = def
  & #title ?~ "Search Results"
  & #footer ?~ embedFooter (fmtResultCount (length names))
  & #fields .~ [embedField (ppType nametype) name | (MemberName _ nametype name _) <- take 10 names]

memid2int :: Key CommunityMember -> Int64
memid2int = unSqlBackendKey . unCommunityMemberKey

int2memid :: Integral int => int -> Key CommunityMember
int2memid = CommunityMemberKey . E.SqlBackendKey . fromIntegral

ppStatus :: CommunityMemberStatus -> T.Text
ppStatus StandardMember = "Standard"
ppStatus Whitelisted    = "Whitelisted"
ppStatus Banned         = "Banned"

ppMemberName :: MemberName -> T.Text
ppMemberName = fmt . ppMemberNamePair
  where fmt (a, b) = a <> ": " <> b

ppMemberNamePair :: MemberName -> (T.Text, T.Text)
ppMemberNamePair (MemberName _ ty name uuid) = (ppType ty, name')
  where
    name' = case ty of
      MinecraftJavaName -> name <> " {" <> (getUUID $ fromJust uuid) <> "}"
      _                 -> name

ppType :: NameType -> T.Text
ppType TwitchName           = "Twitch"
ppType DiscordName          = "Discord"
ppType MinecraftJavaName    = "Minecraft: Java Edition"
ppType MinecraftBedrockName = "Minecraft: Bedrock Edition"

ppCreatedAt :: UTCTime -> T.Text
ppCreatedAt = T.pack . iso8601Show . utctDay

fmtUUID :: UUID -> UUID
fmtUUID (UUID txt) =
  let
    (a, txt'  )  = T.splitAt 8 txt
    (b, txt'' )  = T.splitAt 4 txt'
    (c, txt''')  = T.splitAt 4 txt''
    (d, e   )  = T.splitAt 4 txt'''
  in UUID $ T.intercalate "-" [a,b,c,d,e]
