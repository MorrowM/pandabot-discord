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

-- | An type alias for persistent database operations.
type DatabaseAction a = SqlPersistT (LoggingT (ResourceT IO)) a

connectionString :: Text
connectionString = "database.sqlite"

-- | Run a database action in the IO monad.
db :: DatabaseAction a -> IO a
db =
  runResourceT
    . runStdoutLoggingT
    . withSqliteConn connectionString
    . runSqlConn
