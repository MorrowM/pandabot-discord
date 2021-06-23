{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Pandabot.Bot.Orphans () where

import           Calamity
import           Calamity.Commands.Context (FullContext)
import           Control.Lens
import           Data.Word
import           Database.Persist
import           Database.Persist.Sql

deriving via Word64 instance PersistField (Snowflake a)
deriving via Word64 instance PersistFieldSql (Snowflake a)

instance HasID Channel FullContext where
  getID = getID . view #channel
