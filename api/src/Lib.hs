module Lib where


import           Api
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Logger           ( runStderrLoggingT )
import           Effects.Database.Types
import           Database.Persist
import           Database.Persist.Postgresql
import           Servant
import           Data.Text (Text, pack)
import           Data.Text.Lazy (fromStrict)
import           Data.Text.Lazy.Encoding
import           Network.Wai.Handler.Warp as Warp

connStr = "host=localhost dbname=test user=test password=test port=5432"

server :: ConnectionPool -> Server Api
server pool =
  brandAddH :<|> brandGetH :<|> brandDeleteH :<|> brandUpdateH
  -- :<|> brandUpdateH
  where
    brandAddH newBrand = liftIO $ brandAdd newBrand
    brandGetH id    = liftIO $ brandGet id
    brandDeleteH id    = brandDelete id
    brandUpdateH id brand = brandUpdate id brand

    brandAdd :: Brand -> IO (Maybe (Key Brand))
    brandAdd newBrand = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [BrandName ==. (brandName newBrand)] []
      case exists of
        Nothing -> Just <$> insert newBrand
        Just _ -> return Nothing

    brandGet :: (Key Brand) -> IO (Maybe Brand)
    brandGet id = flip runSqlPersistMPool pool $ do
      mBrand <- selectFirst [BrandId ==. id] []
      return $ entityVal <$> mBrand

    brandDelete :: (Key Brand) -> Handler Text
    brandDelete id = do 
        result <- flip liftSqlPersistMPool pool $ do
            mBrand <- selectFirst [BrandId ==. id] []
            case mBrand of 
                Nothing -> pure $ Left $ "The Band " <> pack (show id) <> " is not present"
                Just _ -> do
                    delete id
                    pure $ Right $ "The Band " <> pack (show id) <> " is deleted" 
        case result of
            Left errorMessage -> throwError $ err404 {errBody = encodeUtf8 $ fromStrict $ errorMessage}
            Right deleteMessage -> pure deleteMessage


    brandUpdate :: (Key Brand) -> Brand -> Handler Text
    brandUpdate id brand = do 
        result <- flip liftSqlPersistMPool pool $ do
            mBrand <- selectFirst [BrandId ==. id] []
            case mBrand of 
                Nothing -> pure $ Left $ "The Band " <> pack (show id) <> " is not present"
                Just _ -> do
                    replace id brand
                    pure $ Right $ "The Band " <> pack (show id) <> " is updated" 
        case result of
            Left errorMessage -> throwError $ err404 {errBody = encodeUtf8 $ fromStrict $ errorMessage}
            Right deleteMessage -> pure deleteMessage


app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkPostgresApp :: IO Application
mkPostgresApp = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> do
    liftIO $ runSqlPersistMPool (runMigration migrateAll) pool
    pure $ app pool

run :: IO ()
run =
  Warp.run 3000 =<< mkPostgresApp
