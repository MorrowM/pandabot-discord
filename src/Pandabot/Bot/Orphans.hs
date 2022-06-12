{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pandabot.Bot.Orphans () where

import Calamity
import Data.Word
import Database.Persist
import Database.Persist.Sql

deriving via Word64 instance PersistField (Snowflake a)
deriving via Word64 instance PersistFieldSql (Snowflake a)
