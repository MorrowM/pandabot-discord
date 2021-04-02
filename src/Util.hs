module Util
  ( wordsWithQuotes
  , myUserId
  , stripEmoji
  , tryGetRoleByName
  , tryGetChannelByName
  , logS
  , tshow
  , isAdmin
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bits              (Bits ((.&.)))
import           Data.Char              (isAscii)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time              (defaultTimeLocale, formatTime,
                                         getCurrentTime)
import           Discord                (Cache (_currentUser), readCache)
import           Discord.Requests       (GuildRequest (GetGuildChannels, GetGuildRoles))
import           Discord.Types          (Channel (ChannelText, channelId, channelName),
                                         ChannelId, GuildId,
                                         GuildMember (memberRoles),
                                         Role (roleId, roleName, rolePerms),
                                         RoleId, Snowflake, User (userId),
                                         UserId)
import           Text.Read              (readMaybe)

import           Types                  (MonadDiscord, NameError (..), getDis,
                                         run)

-- | Split a string into words, taking quotes clauses into account.
wordsWithQuotes :: Text -> [Text]
wordsWithQuotes = concat . wordsEveryOther . T.splitOn "\""
  where
    wordsEveryOther :: [Text] -> [[Text]]
    wordsEveryOther []           = []
    wordsEveryOther [z]          = [T.words z]
    wordsEveryOther (x : y : xs) = T.words x : [y] : wordsEveryOther xs

-- | Retrieve the bot's own user id.
myUserId :: MonadDiscord m => m UserId
myUserId = do
  dis <- getDis
  cache <- liftIO $ readCache dis
  pure $ userId $ _currentUser cache

-- | Strip an emoji into just its name.
stripEmoji :: Text -> Text
stripEmoji emoji = if T.all isAscii emoji
  then T.takeWhile (/= ':') . T.drop 2 $ emoji
  else emoji

-- | Look up a role id by its name.
tryGetRoleByName :: MonadDiscord m => GuildId -> Text -> m (Either (NameError Role) RoleId)
tryGetRoleByName gid name = do
  roles <- run $ GetGuildRoles gid
  pure $ tryGetIdByName roles roleName roleId name

-- | Look up a channel id by its name.
tryGetChannelByName :: MonadDiscord m => GuildId -> Text -> m (Either (NameError (Text, ChannelId)) ChannelId)
tryGetChannelByName gid name = do
  mchans <- run $ GetGuildChannels gid
  let chans = catMaybes $ isText <$> mchans
  pure $ tryGetIdByName chans fst snd name
  where
    isText chan = case chan of
      ChannelText {} -> Just (channelName chan, channelId chan)
      _              -> Nothing

tryGetIdByName :: [a] -> ( a -> Text) -> (a -> Snowflake) -> Text -> Either (NameError a) Snowflake
tryGetIdByName vals toText toId name = case filter ((==name) . toText) vals of
  [] -> case readMaybe (T.unpack name) :: Maybe Snowflake of
    Nothing    -> Left NameNotFound
    Just flake -> Right flake
  [x] -> Right $ toId x
  xs -> Left (NameAmbiguous xs)

-- | Check whether a given member has admin permissions.
isAdmin :: MonadDiscord m => GuildId -> GuildMember -> m Bool
isAdmin gid mem = do
  roles <- run $ GetGuildRoles gid
  pure $ any (`elem` memberRoles mem) (roleId <$> filter isAdminRole roles)
  where
    isSet b n = (b .&. n) == b
    isAdminRole = isSet 8 . rolePerms

-- | Like @show@, but returns @Text@ instead.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Log a string with a timestamp.
logS :: MonadIO m => String -> m ()
logS s = liftIO $ do
  t <- getCurrentTime
  let fmt = formatTime defaultTimeLocale "[%F %T] " t
      output = fmt <> s <> "\n"
  putStr output
  appendFile "log.txt" output
