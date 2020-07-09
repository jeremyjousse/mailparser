module Lib
    ( someFunc
    ) where

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger    (runStderrLoggingT)
import Effects.Database.Types
import Database.Persist
import Database.Persist.Postgresql

connStr = "host=localhost dbname=test user=test password=test port=5432"

someFunc :: IO ()
someFunc = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        brandId <- insert $ Brand "Decathlon" "https://www.decathlon.fr"

        pure ()
