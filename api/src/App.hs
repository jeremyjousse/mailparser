module App where

-- import Database.Persist
import Control.Monad.Trans.Reader
import Database.Persist.Postgresql
import Servant

-- | Alias meaning we can read connection pool and handle servant
-- https://hackage.haskell.org/package/persistent-2.11.0.1/docs/Database-Persist-Sql.html#t:SqlPersistT
type App = ReaderT ConnectionPool Handler
