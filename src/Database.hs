module Database
  ( db
  , DatabaseAction
  )
where

import           Conduit
import           Control.Monad.Logger
import           Data.Text               (Text)
import           Database.Persist.Sqlite

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
