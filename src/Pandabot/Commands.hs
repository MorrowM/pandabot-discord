{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Pandabot.Commands
  ( registerBotCommands
  , countPoints
  ) where

import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           CalamityCommands          (ConstructContext, ParsePrefix)
import           CalamityCommands.Check    (buildCheck)
import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Flags
import qualified Data.List                 as L
import qualified Data.Map                  as Map
import           Data.Maybe
import           Data.Ord
import           Data.Text                 (Text, pack)
import qualified Data.Text                 as T
import           Data.Traversable
import           Database.Persist          as DB
import qualified Polysemy                  as P
import qualified Polysemy.Fail             as P
import qualified Polysemy.Time             as P
import           TextShow

import           Pandabot.Database
import           Pandabot.Schema
import           Pandabot.Util

-- | Register all the bot commands
registerBotCommands ::
  ( BotC r
  , P.Members
    [ ParsePrefix Message
    , Persistable
    , P.Fail
    , P.GhcTime
    , ConstructContext Message FullContext IO ()
    ] r
  ) => P.Sem r ()
registerBotCommands = void $ addCommands $ do
  admin <- isAdmin
  void helpCommand
  requires [admin] $ help (const "Manage role buttons")
    $ group "button" $ do
    void @_ @(Command FullContext) $ help (const "Create a role button")
      $ command @'[GuildChannel, RawEmoji, Role, Text] "add" $ \ctx chan emoj role txt -> void $ do
      sameGuild ctx chan
      Right msg <- invoke $ CreateMessage chan (def & #content ?~ txt)
      let newButton = Button (getID chan) (getID msg) (pack $ show emoj) (getID role)
      info $ "Adding button " <> showtl (FromStringShow newButton)
      db_ $ insert newButton
      invoke $ CreateReaction chan msg emoj

    void $ help (const "Insert a role button into an existing message")
      $ command @'[GuildChannel, RawEmoji, Role, Snowflake Message] "insert" $ \ctx chan emoj role msg -> void $ do
      sameGuild ctx chan
      let newButton = Button (getID chan) msg (pack $ show emoj) (getID role)
      info $ "Inserting button " <> showtl (FromStringShow newButton)
      db_ $ insert newButton
      invoke $ CreateReaction chan msg emoj

    void $ help (const "Remove a role button")
      $ command @'[GuildChannel, RawEmoji, Snowflake Message] "remove" $ \ctx chan emoj msg -> void $ do
      sameGuild ctx chan
      info $ "Deleting button " <> showtl (chan, emoj, msg)
      db_ $ deleteWhere [ButtonChannel ==. getID chan, ButtonMessage ==. msg, ButtonEmoji ==. showt emoj]
      invoke $ DeleteOwnReaction chan msg emoj

  void $ help (const "How many shoots do you have?")
    $ command @'[Maybe User] "shoots" $ \ctx muser -> do
    Just gld <- pure (ctx ^. #guild)
    points <- countPoints (fromMaybe (ctx ^. #user) muser) gld
    void . tellt ctx $ (fromMaybe (ctx ^. #user) muser ^. #username) <> " has " <> (showPoints points ^. lazy) <> "."

  void $ help (const "See all the awesomest pandas")
    $ command @'[] "leaderboard" $ \ctx -> do
    Just gld <- pure (ctx ^. #guild)
    messagePointsRaw <- db $ selectList [MessagePointGuild ==. getID gld] [Asc MessagePointAssignedTo]
    freePointsRaw <- db $ selectList [FreePointGuild ==. getID gld] [Asc FreePointAssignedTo]
    let messagePointsMap
          = Map.fromListWith (+)
          . fmap (,1)
          $ messagePointAssignedTo . entityVal <$> messagePointsRaw
        freePointsMap
          = Map.fromListWith (+)
          $ (freePointAssignedTo &&& freePointAmount) . entityVal <$> freePointsRaw
        pointsMap = Map.unionWith (+) messagePointsMap freePointsMap
        topFive = take 5 . L.sortOn (Down . snd) . Map.toList $ pointsMap
    points <- for topFive $ \(u, p) -> do
      Just usr <- upgrade u
      pure (usr ^. #username . strict, p)

    let txt = view lazy $ T.unlines [showt i <> ". " <> nm <> ": " <> showPoints p | (i, (nm, p)) <- zip [(1 :: Int)..] points]
    void $ if null points
      then tellt ctx "No one has any points yet!"
      else tell @Embed ctx $ def
        & #title ?~ "Leaderboard"
        & #description ?~ txt

  void $ requires [admin] $ help (const "Award a panda some delicious bamboo")
    $ command @'[Member, Named "shoots" (Maybe Int)] "award" $ \(ctx :: FullContext) mem mamnt -> do
    time <- P.now
    let amnt = fromMaybe 1 mamnt
        point = FreePoint (mem ^. #guildID) (ctx ^. #user . #id) (mem ^. #id) time amnt
    db_ $ insert point
    points <- countPoints mem mem
    void . tellt ctx $
      mem ^. to mention
      <> " has been awarded " <> (showPoints amnt ^. lazy) <> " for being an awesome panda!\nThey now have "
      <> (showPoints points ^. lazy)  <> " total."

-- | Create a `Check` for whether the user invoking the
-- command is an administrator.
isAdmin :: BotC r => P.Sem r (Check FullContext)
isAdmin = buildCheck "requires admin" $ \ctx -> do
  case ctx ^. #member of
    Nothing -> pure $ Just "This command can only be run in a server"
    Just mem -> do
      perms <- permissionsIn' (mem ^. #guildID) mem
      pure $ if perms `containsAll` administrator
        then Nothing
        else Just "User must be an administrator"

sameGuild :: (BotC r, P.Member P.Fail r) => FullContext -> GuildChannel -> P.Sem r ()
sameGuild ctx chan = when (Just (getID @Guild chan) == (getID <$> ctx ^. #guild)) $ do
        fire $ customEvt (CtxCommandError ctx $ CheckError "same guild" "Cannot modify buttons in other guilds")

countPoints ::
  ( BotC r
  , P.Member Persistable r
  , HasID User u
  , HasID Guild g
  ) => u -> g -> P.Sem r Int
countPoints usr gld = (+) <$> msgPoints <*> freePoints
  where
    msgPoints = db $ DB.count [MessagePointAssignedTo ==. getID usr, MessagePointGuild ==. getID gld]
    freePoints = sum . fmap (getFreePointAmnt . entityVal) <$> db (selectList [FreePointAssignedTo ==. getID usr, FreePointGuild ==. getID gld] [])
    getFreePointAmnt (FreePoint _ _ _ _ amnt) = amnt
