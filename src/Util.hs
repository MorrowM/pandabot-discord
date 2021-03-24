module Util
  ( wordsWithQuotes
  , myUserId
  , stripEmoji
  , inGuild
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

import           Types                  (Handler, NameError (..), getDis, run)

wordsWithQuotes :: Text -> [Text]
wordsWithQuotes = concat . wordsEveryOther . T.splitOn "\""
  where
    wordsEveryOther :: [Text] -> [[Text]]
    wordsEveryOther []           = []
    wordsEveryOther [z]          = [T.words z]
    wordsEveryOther (x : y : xs) = T.words x : [y] : wordsEveryOther xs

myUserId :: Handler UserId
myUserId = do
  dis <- getDis
  cache <- liftIO $ readCache dis
  pure $ userId $ _currentUser cache

stripEmoji :: Text -> Text
stripEmoji emoji = if T.all isAscii emoji
  then T.takeWhile (/= ':') . T.drop 2 $ emoji
  else emoji

inGuild :: Maybe GuildId -> (GuildId -> Handler ()) -> Handler ()
inGuild = flip $ maybe (pure ())

tryGetRoleByName :: GuildId -> Text -> Handler (Either (NameError Role) RoleId)
tryGetRoleByName gid name = do
  roles <- run $ GetGuildRoles gid
  pure $ tryGetIdByName roles roleName roleId name

tryGetChannelByName :: GuildId -> Text -> Handler (Either (NameError (Text, ChannelId)) ChannelId)
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

isAdmin :: GuildId -> GuildMember -> Handler Bool
isAdmin gid mem = do
  roles <- run $ GetGuildRoles gid
  pure $ any (`elem` memberRoles mem) (roleId <$> filter isAdminRole roles)
  where
    isSet b n = (b .&. n) == b
    isAdminRole = isSet 8 . rolePerms

tshow :: Show a => a -> Text
tshow = T.pack . show

logS :: MonadIO m => String -> m ()
logS s = liftIO $ do
  t <- getCurrentTime
  let fmt = formatTime defaultTimeLocale "[%F %T] " t
      output = fmt <> s <> "\n"
  putStr output
  appendFile "log.txt" output
