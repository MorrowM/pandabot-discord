module Database
  ( db
  , DatabaseAction
  )
where

import Conduit ( runResourceT, ResourceT )
import Control.Monad.Logger ( runStdoutLoggingT, LoggingT )
import Data.Text (Text ())
import Database.Persist.Sqlite
    ( runSqlConn, withSqliteConn, SqlPersistT )

type DatabaseAction a = SqlPersistT (LoggingT (ResourceT IO)) a

connectionString :: Text
connectionString = "database.sqlite"

db :: DatabaseAction a -> IO a
db =
  runResourceT
    . runStdoutLoggingT
    . withSqliteConn connectionString
    . runSqlConn