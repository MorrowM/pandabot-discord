{-# LANGUAGE OverloadedStrings #-}
module Instances where

import Database.Persist.Sql
import Data.Text ( pack )
import Discord.Types


instance PersistField Snowflake where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 n) = Right . toEnum . fromIntegral $ n
  fromPersistValue x = Left $ "Error: could not convert a database value to Snowflake, expected Int64, received: " <> pack (show x)

instance PersistFieldSql Snowflake where
  sqlType _ = SqlInt64