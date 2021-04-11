module Commands where

import           Calamity
import           Calamity.Commands
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Text         (Text, pack)
import           Database.Persist
import qualified Polysemy          as P
import           TextShow

import           Database
import           Schema
import           Util


registerBotCommands :: (BotC r, P.Member ParsePrefix r, P.Member Persistable r) => P.Sem r ()
registerBotCommands = void $ addCommands $ do
  void helpCommand
  group "button" $ do
    void $ command @'[GuildChannel, RawEmoji, Role, Text] "add" $ \_ctx chan emoj role txt -> do
      Right msg <- invoke $ CreateMessage chan (def & #content ?~ txt)
      let newButton = Button (getID chan) (getID msg) (pack $ show emoj) (getID role)
      info $ "Inserting button " <> showtl (FromStringShow newButton)
      db_ $ insert newButton
      void . invoke $ CreateReaction chan msg emoj

    void $ command @'[GuildChannel, RawEmoji, Role, Snowflake Message] "insert" $ \_ctx chan emoj role msg -> do
      let newButton = Button (getID chan) msg (pack $ show emoj) (getID role)
      info $ "Inserting button " <> showtl (FromStringShow newButton)
      db_ $ insert newButton
      void . invoke $ CreateReaction chan msg emoj

    void $ command @'[GuildChannel, RawEmoji, Snowflake Message] "reomve" $ \_ctx chan emoj msg -> do
      info $ "Deleting button " <> showtl (chan, emoj, msg)
      db_ $ deleteWhere [ButtonChannel ==. getID chan, ButtonMessage ==. msg, ButtonEmoji ==. showt emoj]
      void . invoke $ DeleteOwnReaction chan msg emoj


