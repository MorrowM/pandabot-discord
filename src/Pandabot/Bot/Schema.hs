{-# LANGUAGE DataKinds                  #-}
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
module Pandabot.Bot.Schema where

import           Calamity
import           Data.Text            (Text)
import           Data.Time
import           Database.Persist.TH
import           Pandabot.Bot.Orphans ()

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Button
    channel (Snowflake Channel)
    message (Snowflake Message)
    emoji Text
    role (Snowflake Role)
    deriving Show

  MessagePoint
    message (Snowflake Message)
    guild (Snowflake Guild)
    assignedBy (Snowflake User)
    assignedTo (Snowflake User)
    assignedAt UTCTime
    deriving Show

  FreePoint
    guild (Snowflake Guild)
    assignedBy (Snowflake User)
    assignedTo (Snowflake User)
    assignedAt UTCTime
    amount Int
    deriving Show
|]
