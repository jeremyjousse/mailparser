module Controller.Gmail where

import Api
import App
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader
import Data.Either (rights)
import Data.Foldable (traverse_)
import Data.Text
  ( Text,
    pack,
  )
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Database.Persist
import Database.Persist.Postgresql
import qualified Effects.Database.Types as Db
import Gmail
import Servant

controller :: ServerT Api App
controller = brandAdd :<|> brandGet :<|> brandDelete :<|> brandUpdate :<|> updateGmailMessages

brandAdd :: Db.Brand -> App (Maybe (Key Db.Brand))
brandAdd newBrand = do
  exists <- runDB $ selectFirst [Db.BrandName ==. Db.brandName newBrand] []
  case exists of
    Nothing -> runDB $ Just <$> insert newBrand
    Just _ -> return Nothing

brandGet :: Key Db.Brand -> App (Maybe Db.Brand)
brandGet id = do
  mBrand <- runDB $ selectFirst [Db.BrandId ==. id] []
  return $ entityVal <$> mBrand

brandDelete ::  Key Db.Brand -> App Text
brandDelete id = do
  result <- do
    mBrand <- runDB $ selectFirst [Db.BrandId ==. id] []
    case mBrand of
      Nothing ->
        pure $ Left $ "The brand " <> pack (show id) <> " is not present"
      Just _ -> do
        runDB $ delete id
        pure $ Right $ "The brand " <> pack (show id) <> " is deleted"
  case result of
    Left errorMessage ->
      throwError $ err404 {errBody = encodeUtf8 $ fromStrict $ errorMessage}
    Right deleteMessage -> pure deleteMessage

brandUpdate :: Key Db.Brand -> Db.Brand -> App Text
brandUpdate id brand = do
  result <- do
    mBrand <- runDB $ selectFirst [Db.BrandId ==. id] []
    case mBrand of
      Nothing ->
        pure $ Left $ "The brand " <> pack (show id) <> " is not present"
      Just _ -> do
        runDB $ replace id brand
        pure $ Right $ "The brand " <> pack (show id) <> " is updated"
  case result of
    Left errorMessage ->
      throwError $ err404 {errBody = encodeUtf8 $ fromStrict errorMessage}
    Right updateMessage -> pure updateMessage

updateGmailMessages :: App Text
updateGmailMessages = do
  messageList <- liftIO listMessages
  case messageList of
    Left gmailError -> throwError $ err500 {errBody = encodeUtf8 $ fromStrict $ pack $ show gmailError}
    Right messageList -> do
      foo :: [Either GmailError Gmail.GmailMessage] <- traverse updateGmailMessage messageList
      pure $ pack $ show $ rights foo

updateGmailMessage :: Gmail.GmailMessage -> App (Either GmailError Gmail.GmailMessage)
updateGmailMessage gmailMessage@Gmail.GmailMessage {threadId} = do
  eitherGmailMessageDetail <- liftIO $ messageDetail threadId
  case eitherGmailMessageDetail of
    Left gmailError -> pure $ Left gmailError
    Right gmailMessageDetail -> do
      dbGmailMessage <- runDB $ insert $ mappGmailMessageDetailHttpToGmailMessageDb gmailMessageDetail
      traverse_ updateGmailMessageHeader $ mappGmailMessageDetailHttpToGmailMessageHeaderDb gmailMessageDetail dbGmailMessage
      liftIO $ updateMessageLabels threadId GmailMessageLabelUpdateRequest {removeLabelIds = ["UNREAD", "INBOX"]}
      pure $ Right gmailMessage

updateGmailMessageHeader :: Db.GmailMessageHeader -> App (Key Db.GmailMessageHeader)
updateGmailMessageHeader dbGmailMessageHeader = runDB $ insert dbGmailMessageHeader

runDB :: SqlPersistT IO a -> App a
runDB query = do
  pool <- ask
  liftIO $ runSqlPool query pool

mappGmailMessageDetailHttpToGmailMessageDb :: GmailMessageDetail -> Db.GmailMessage
mappGmailMessageDetailHttpToGmailMessageDb GmailMessageDetail {id, threadId, labelIds, snippet, payload, sizeEstimate, historyId, internalDate} =
  Db.GmailMessage
    { Db.gmailMessageThreadId = pack threadId,
      Db.gmailMessageSnippet = pack snippet,
      Db.gmailMessageSizeEstimate = sizeEstimate,
      Db.gmailMessageHistoryId = pack historyId,
      Db.gmailMessageInternalDate = pack internalDate,
      Db.gmailMessageMimeType = pack (extractMimeType payload)
    }

mappGmailMessageDetailHttpToGmailMessageHeaderDb :: GmailMessageDetail -> Key Db.GmailMessage -> [Db.GmailMessageHeader]
mappGmailMessageDetailHttpToGmailMessageHeaderDb gmailMessageDetail dbGmailMessageKey =
  map (\headers -> httpHeaderToBddHeader headers dbGmailMessageKey) messageHeaders
  where
    messageHeaders = headers $ payload gmailMessageDetail
    httpHeaderToBddHeader headers dbGmailMessage =
      Db.GmailMessageHeader
        { Db.gmailMessageHeaderName = pack $ name headers,
          Db.gmailMessageHeaderValue = pack $ value headers,
          Db.gmailMessageHeaderGmailMessage = dbGmailMessageKey
        }

-- TODO Is this the best way to do, can it be done nativeliy on line 47?
extractMimeType :: GmailMessagePayload -> String
extractMimeType GmailMessagePayload {mimeType} = mimeType
