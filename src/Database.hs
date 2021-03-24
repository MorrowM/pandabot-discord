module Database
  ( db
  , DatabaseAction
  )
where

import           Conduit                 (ResourceT, runResourceT)
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Text               (Text)
import           Database.Persist.Sqlite (SqlPersistT, runSqlConn,
                                          withSqliteConn)

type DatabaseAction a = SqlPersistT (LoggingT (ResourceT IO)) a

connectionString :: Text
connectionString = "database.sqlite"

db :: DatabaseAction a -> IO a
db =
  runResourceT
    . runStdoutLoggingT
    . withSqliteConn connectionString
    . runSqlConn
