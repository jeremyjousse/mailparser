module Lib (runApp) where

import Api
import App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader
import Controller.Gmail
import Database.Persist.Postgresql
import Effects.Database.Types
import Network.Wai.Handler.Warp as Warp
import Servant

-- For more inspiration see
-- https://github.com/parsonsmatt/servant-persistent

connStr =
  "host=localhost dbname=mailparser user=postgres password=postgres port=5432"

runApp :: IO ()
runApp = do
  application <- mkPostgresApp
  Warp.run 3000 application

mkPostgresApp :: IO Application
mkPostgresApp = runStderrLoggingT $
  withPostgresqlPool connStr 10 $ \pool -> do
    liftIO $ do
      flip runSqlPersistMPool pool do
        runMigration migrateAll
        pure $ app pool

app :: ConnectionPool -> Application
app pool = serve api (server pool)

server :: ConnectionPool -> Server Api
server pool = hoistServer api (convertApp pool) controller

convertApp :: ConnectionPool -> App a -> Handler a
convertApp pool application = runReaderT application pool
