{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant pure" #-}
module Pandabot.Buttons
  ( registerButtonCommands
  , registerButtonPressHandler
  ) where

import Calamity hiding ( Button )
import Calamity.Cache.Eff
import Calamity.Commands
import Calamity.Commands.Context ( FullContext )
import Calamity.Interactions qualified as I
import Control.Monad
import Data.Default
import Data.Foldable
import Data.Text ( Text, pack )
import Data.Vector.Unboxing qualified as V
import Database.Persist
import Optics
import Pandabot.Bot.Database
import Pandabot.Bot.Schema
import Pandabot.Bot.Util
import Polysemy qualified as P
import Polysemy.Fail qualified as P
import Polysemy.NonDet qualified as P
import TextShow

registerButtonCommands ::
  ( BotC r
  , P.Members
   '[ Persistable
    ] r
  ) => Check FullContext -> P.Sem (DSLState FullContext r) ()
registerButtonCommands admin = requires [admin] $ help (const "Manage buttons.") $ hide
    $ group "button" do
    void $ help (const "Create a role button")
      $ command @'[GuildChannel, RawEmoji, Role, Text] "add" $ \_ctx chan emoj role txt -> void do
      Right msg <- invoke $ CreateMessage chan (def & #content ?~ txt)
      let newButton = Button (getID chan) (getID msg) (pack $ show emoj) (getID role)
      info $ "Adding button " <> showt (FromStringShow newButton)
      db_ $ insert newButton
      let view = I.row do
            btn <- I.button' (#emoji ?~ emoj)
            pure btn
      I.runView view (tell $ getID @Channel chan) \a -> do
        when a $ do
          usr <- I.getInteractionUser
          invoke_ $ AddGuildMemberRole (getID @Guild chan) usr role
          void $ I.respondEphemeral $ "Added role " <> role ^. #name
      -- invoke $ CreateReaction chan msg emoj

    void $ help (const "Insert a role button into an existing message")
      $ command @'[GuildChannel, RawEmoji, Role, Snowflake Message] "insert" \_ctx chan emoj role msg -> void do
      let newButton = Button (getID chan) msg (pack $ show emoj) (getID role)
      info $ "Inserting button " <> showt (FromStringShow newButton)
      db_ $ insert newButton
      invoke $ CreateReaction chan msg emoj

    void $ help (const "Remove a role button")
      $ command @'[GuildChannel, RawEmoji, Snowflake Message] "remove" \_ctx chan emoj msg -> void do
      info $ "Deleting button " <> showt (chan, emoj, msg)
      db_ $ deleteWhere [ButtonChannel ==. getID chan, ButtonMessage ==. msg, ButtonEmoji ==. showt emoj]
      invoke $ DeleteOwnReaction chan msg emoj

registerButtonPressHandler ::
  ( BotC r
  , P.Members
   '[ Persistable
   , P.Fail
    ] r
  ) => P.Sem r ()
registerButtonPressHandler = void $ P.runNonDetMaybe $ react @'RawMessageReactionAddEvt \rinfo -> do
    Just gid <- pure (rinfo ^. #guildID)
    Just myUid <- fmap getID <$> getBotUser
    guard $ myUid /= (rinfo ^. #userID)
    btns <- fmap (fmap entityVal) $ db
      $ selectList [ ButtonMessage ==. (rinfo ^. #messageID)
                   , ButtonEmoji ==. showt (rinfo ^. #emoji)
                   ] []
    for_ btns \btn -> do
      Right mem <- invoke $ GetGuildMember gid (rinfo ^. #userID)
      void if buttonRole btn `V.elem` (mem ^. #roles)
        then invoke $ RemoveGuildMemberRole gid (mem ^. #id) (buttonRole btn)
        else invoke $ AddGuildMemberRole    gid (mem ^. #id) (buttonRole btn)
      invoke $ DeleteUserReaction (rinfo ^. #channelID) (rinfo ^. #messageID) (rinfo ^. #emoji) mem
