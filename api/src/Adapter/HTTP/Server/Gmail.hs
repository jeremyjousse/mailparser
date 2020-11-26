module Adapter.HTTP.Server.Gmail where

import qualified Adapter.HTTP.Client.Gmail as GMC
import Adapter.HTTP.Server.Api
import qualified Adapter.PostgreSQL.Models as Db
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
import qualified Gmail.Types as GMT
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

brandDelete :: Key Db.Brand -> App Text
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
  messageList <- liftIO GMC.listMessages
  case messageList of
    Left gmailError -> throwError $ err500 {errBody = encodeUtf8 $ fromStrict $ pack $ show gmailError}
    Right messageList -> do
      foo :: [Either GMC.GmailError GMC.GmailMessage] <- traverse updateGmailMessage messageList
      pure $ pack $ show $ rights foo

updateGmailMessage :: GMC.GmailMessage -> App (Either GMC.GmailError GMC.GmailMessage)
updateGmailMessage gmailMessage@GMC.GmailMessage {threadId} = do
  eitherGmailMessageDetail <- liftIO $ GMC.messageDetail threadId
  case eitherGmailMessageDetail of
    Left gmailError -> pure $ Left gmailError
    Right gmailMessageDetail -> do
      dbGmailMessage <- runDB $ insert $ mappGmailMessageDetailHttpToGmailMessageDb gmailMessageDetail
      traverse_ updateGmailMessageHeader $ mappGmailMessageDetailHttpToGmailMessageHeaderDb gmailMessageDetail dbGmailMessage
      liftIO $ GMC.updateMessageLabels threadId GMT.GmailMessageLabelUpdateRequest {removeLabelIds = ["UNREAD", "INBOX"]}
      pure $ Right gmailMessage

updateGmailMessageHeader :: Db.EmailMessageHeader -> App (Key Db.EmailMessageHeader)
updateGmailMessageHeader dbGmailMessageHeader = runDB $ insert dbGmailMessageHeader

runDB :: SqlPersistT IO a -> App a
runDB query = do
  pool <- ask
  liftIO $ runSqlPool query pool

mappGmailMessageDetailHttpToGmailMessageDb :: GMT.GmailMessageDetail -> Db.EmailMessage
mappGmailMessageDetailHttpToGmailMessageDb GMT.GmailMessageDetail {id, threadId, labelIds, snippet, payload, sizeEstimate, historyId, internalDate} =
  Db.EmailMessage
    { Db.emailMessageThreadId = pack threadId,
      Db.emailMessageSnippet = pack snippet,
      Db.emailMessageSizeEstimate = sizeEstimate,
      Db.emailMessageHistoryId = pack historyId,
      Db.emailMessageInternalDate = pack internalDate,
      Db.emailMessageMimeType = pack (extractMimeType payload)
    }

mappGmailMessageDetailHttpToGmailMessageHeaderDb :: GMT.GmailMessageDetail -> Key Db.EmailMessage -> [Db.EmailMessageHeader]
mappGmailMessageDetailHttpToGmailMessageHeaderDb gmailMessageDetail dbGmailMessageKey =
  map (\headers -> httpHeaderToBddHeader headers dbGmailMessageKey) messageHeaders
  where
    messageHeaders = GMT.headers $ GMT.payload gmailMessageDetail
    httpHeaderToBddHeader headers dbGmailMessage =
      Db.EmailMessageHeader
        { Db.emailMessageHeaderName = pack $ GMT.name headers,
          Db.emailMessageHeaderValue = pack $ GMT.value headers,
          Db.emailMessageHeaderEmailMessage = dbGmailMessageKey
        }

-- TODO Is this the best way to do, can it be done nativeliy on line 47?
extractMimeType :: GMT.GmailMessagePayload -> String
extractMimeType GMT.GmailMessagePayload {mimeType} = mimeType
