{-# LANGUAGE OverloadedStrings #-}

module Database
  ( db
  , DatabaseAction
  )
where

import Conduit
import Control.Monad.Logger
import Data.Text (Text ())
import Database.Persist.Sqlite

type DatabaseAction a = SqlPersistT (LoggingT (ResourceT IO)) a

connectionString :: Text
connectionString = "database.sqlite"

db :: DatabaseAction a -> IO a
db =
  runResourceT
    . runStdoutLoggingT
    . withSqliteConn connectionString
    . runSqlConn