{-# OPTIONS_GHC -Wno-orphans #-}
module Instances () where

import           Data.Generics.Labels ()
import           Data.Text            (pack)
import           Database.Persist.Sql (PersistField (..), PersistFieldSql (..),
                                       PersistValue (PersistInt64),
                                       SqlType (SqlInt64))
import           Discord.Types        (Snowflake)


instance PersistField Snowflake where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 n) = Right . toEnum . fromIntegral $ n
  fromPersistValue x = Left $ "Error: could not convert a database value to Snowflake, expected Int64, received: " <> pack (show x)

instance PersistFieldSql Snowflake where
  sqlType _ = SqlInt64
