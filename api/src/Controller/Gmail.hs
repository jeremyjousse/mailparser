module Controller.Gmail where

import Api
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Either (rights)
import Data.Text
  ( Text,
    pack,
  )
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding
import Database.Persist
import Database.Persist.Postgresql
import qualified Effects.Database.Types as Db (GmailMessage (..))
import Gmail
import Network.Wai.Handler.Warp as Warp
import Servant

updateGmailMessages :: ConnectionPool -> Handler Text
updateGmailMessages pool = do
  messageList <- liftIO listMessages
  case messageList of
    Left gmailError -> throwError $ err500 {errBody = encodeUtf8 $ fromStrict $ pack $ show gmailError}
    Right messageList -> liftIO $ do
      foo :: [Either GmailError GmailMessage] <- traverse (updateGmailMessage pool) messageList
      pure $ pack $ show $ rights foo

updateGmailMessage :: ConnectionPool -> GmailMessage -> IO (Either GmailError GmailMessage)
updateGmailMessage pool gmailMessage@GmailMessage {threadId} = do
  eitherGmailMessageDetail <- messageDetail threadId
  case eitherGmailMessageDetail of
    Left gmailError -> pure $ Left gmailError
    Right gmailMessageDetail -> do
      flip liftSqlPersistMPool pool $ insert $ mappGmailMessageDetailHttpToGmailMessageDb gmailMessageDetail
      updateMessageLabels threadId GmailMessageLabelUpdateRequest {removeLabelIds = ["UNREAD", "INBOX"]}
      pure $ Right gmailMessage

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

-- TODO Is this the best way to do, can it be done nativeliy on line 47?
extractMimeType :: GmailMessagePayload -> String
extractMimeType GmailMessagePayload {mimeType} = mimeType
