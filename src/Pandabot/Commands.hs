module Pandabot.Commands
( registerBotCommands
) where

import           Calamity
import           Calamity.Commands       as C
import           Calamity.Commands.Check
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Flags
import qualified Data.List               as L
import           Data.Ord
import           Data.Text               (Text, pack)
import qualified Data.Text               as T
import           Data.Traversable
import           Database.Persist        as DB
import qualified Polysemy                as P
import qualified Polysemy.Fail           as P
import           TextShow

import           Data.Maybe
import           Pandabot.Database
import           Pandabot.Schema
import           Pandabot.Util

-- | Register all the bot commands
registerBotCommands ::
  ( BotC r
  , P.Member ParsePrefix r
  , P.Member Persistable r
  , P.Member P.Fail r
  ) => P.Sem r ()
registerBotCommands = void $ addCommands $ do
  admin <- isAdmin
  void helpCommand
  requires [admin] $ group "button" $ do
    void $ command @'[GuildChannel, RawEmoji, Role, Text] "add" $ \ctx chan emoj role txt -> void $ do
      sameGuild ctx chan
      Right msg <- invoke $ CreateMessage chan (def & #content ?~ txt)
      let newButton = Button (getID chan) (getID msg) (pack $ show emoj) (getID role)
      info $ "Adding button " <> showtl (FromStringShow newButton)
      db_ $ insert newButton
      invoke $ CreateReaction chan msg emoj

    void $ command @'[GuildChannel, RawEmoji, Role, Snowflake Message] "insert" $ \ctx chan emoj role msg -> void $ do
      sameGuild ctx chan
      let newButton = Button (getID chan) msg (pack $ show emoj) (getID role)
      info $ "Inserting button " <> showtl (FromStringShow newButton)
      db_ $ insert newButton
      invoke $ CreateReaction chan msg emoj

    void $ command @'[GuildChannel, RawEmoji, Snowflake Message] "reomve" $ \ctx chan emoj msg -> void $ do
      sameGuild ctx chan
      info $ "Deleting button " <> showtl (chan, emoj, msg)
      db_ $ deleteWhere [ButtonChannel ==. getID chan, ButtonMessage ==. msg, ButtonEmoji ==. showt emoj]
      invoke $ DeleteOwnReaction chan msg emoj

  void $ help (const "How many shoots do you have?") $ command @'[Maybe User] "shoots" $ \ctx muser -> do
    Just gld <- pure (ctx ^. #guild)
    points <- db $ DB.count [PointAssignedTo ==. maybe (ctx ^. #user . to getID) getID muser, PointGuild ==. getID gld]
    void . tellt ctx $ (fromMaybe (ctx ^. #user) muser ^. #username) <> " has " <> (showPoints points ^. lazy) <> "."

  void $ command @'[] "leaderboard" $ \ctx -> do
    Just gld <- pure (ctx ^. #guild)
    pointsRaw <- db $ selectList [PointGuild ==. getID gld] [Asc PointAssignedTo]
    let pointsList
          = take 5
          . L.sortOn (Down . snd)
          . fmap (head &&& length)
          . L.group
          $ pointAssignedTo . entityVal <$> pointsRaw
    points <- for pointsList $ \(u, p) -> do
      Just usr <- upgrade u
      pure (usr ^. #username . strict, p)

    void $ if null points
      then tellt ctx "No one has any points yet!"
      else tellt ctx $ view lazy $ T.unlines [showt i <> ". " <> nm <> ": " <> showPoints p | (i, (nm, p)) <- zip [(1 :: Int)..] points]

-- | Create a `Check` for whether the user invoking the
-- command is an administrator.
isAdmin :: BotC r => P.Sem r Check
isAdmin = buildCheck "requires admin" $ \ctx -> do
  P.embed $ print ctx
  case ctx ^. #member of
    Nothing -> pure $ Just "This command can only be run in a server"
    Just mem -> do
      perms <- permissionsIn' (mem ^. #guildID) mem
      pure $ if andFlags perms administrator `containsAll` administrator
        then Nothing
        else Just "User must be an administrator"

sameGuild :: (BotC r, P.Member P.Fail r) => C.Context -> GuildChannel -> P.Sem r ()
sameGuild ctx chan = when (Just (getID @Guild chan) == (getID <$> ctx ^. #guild)) $ do
        fire $ customEvt @"command-error" (CheckError "same guild" "Cannot modify buttons in other guilds")
