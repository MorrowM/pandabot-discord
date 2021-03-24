{-# OPTIONS_GHC -Wno-orphans #-}
module Instances () where

import Database.Persist.Sql
    ( PersistField(..),
      PersistFieldSql(..),
      SqlType(SqlInt64),
      PersistValue(PersistInt64) )
import Discord.Types ( Snowflake )

import Util ( tshow )


instance PersistField Snowflake where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 n) = Right . toEnum . fromIntegral $ n
  fromPersistValue x = Left $ "Error: could not convert a database value to Snowflake, expected Int64, received: " <> tshow x

instance PersistFieldSql Snowflake where
  sqlType _ = SqlInt64