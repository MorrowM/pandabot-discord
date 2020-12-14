{-# LANGUAGE OverloadedStrings #-}
module NotifPoints where

import Data.Text (Text)
import Database.Persist.Sql ( (==.), PersistQueryRead(count) )
import Discord.Types ( GuildId, User(userId, userName) )

import Commands ( NotifPointsComm(..) )
import Schema
    ( EntityField(NotifPointGuild, NotifPointAssignedTo) )
import Types ( runDB, Handler )
import Util ( tshow )

runNotifPointsComm :: NotifPointsComm -> GuildId -> User -> (Text -> Handler ()) -> Handler (Either NotifPointsCommError ())
runNotifPointsComm comm gid usr reply = do
  case comm of
    ViewSelf -> do
      points <- runDB $ count [NotifPointAssignedTo ==. userId usr, NotifPointGuild ==. gid]
      reply $ userName usr <> " has " <> tshow points <> " points"
      pure $ Right ()

data NotifPointsCommError