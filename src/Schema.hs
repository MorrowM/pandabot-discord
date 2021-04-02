{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}

module Schema where

import           Data.Text           (Text)
import           Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase,
                                      share, sqlSettings)
import           Discord.Types       (ChannelId, GuildId, MessageId, RoleId,
                                      UTCTime, UserId)
import           Instances           ()

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Button
    channel ChannelId
    message MessageId
    emoji Text
    role RoleId
    deriving Show

  NotifPoint
    message MessageId
    guild GuildId
    assignedBy UserId
    assignedTo UserId
    assignedAt UTCTime
|]
