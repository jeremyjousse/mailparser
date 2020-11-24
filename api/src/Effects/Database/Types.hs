{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Effects.Database.Types where

import Data.Aeson
import Data.Text
import Database.Persist ()
import Database.Persist.Sql
  ( Migration,
    addMigration,
  )
import Database.Persist.TH

-- import qualified Model.Brand as Brand

{-
add GmailMessage (id, threadId, status[new, imported, ...], snippet, mimeType, sizeEstimate,historyId, internalDate)
add GmailMessageHeader (id, gmailMessagePartId, type ["message", "part"],  name, value)
add GmailMessagePart (id, gmailMessageId, partId, mimeType, )
add GmailMessagePartBody (id, GmailMessagePartId, size, data )
-}

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Brand
      name Text
      url Text
      deriving Show
  GmailMessage
      threadId Text
      snippet Text
      sizeEstimate Int
      historyId Text
      internalDate Text
      mimeType Text
      deriving Show
  GmailMessageHeader
      name Text
      value Text
      gmailMessage GmailMessageId
  GmailMessagePart
      partId Int
      data Text
      mimeType Text
      gmailMessage GmailMessageId
  Sender
      from Text
      returnPath Text
      brandId BrandId Maybe
      deriving Show
  |]

instance FromJSON Brand where
  parseJSON = withObject "Brand" $ \brandJson -> do
    name <- brandJson .: "name"
    url <- brandJson .: "url"
    pure Brand {brandName = name, brandUrl = url}

instance ToJSON Brand where
  toJSON Brand {brandName, brandUrl} =
    object ["name" .= brandName, "url" .= brandUrl]

instance FromJSON GmailMessage where
  parseJSON = withObject "GmailMessage" $ \gmailMessageJson -> do
    threadId <- gmailMessageJson .: "threadId"
    snippet <- gmailMessageJson .: "snippet"
    sizeEstimate <- gmailMessageJson .: "sizeEstimate"
    historyId <- gmailMessageJson .: "historyId"
    internalDate <- gmailMessageJson .: "internalDate"
    mimeType <- gmailMessageJson .: "mimeType"
    pure
      GmailMessage
        { gmailMessageThreadId = threadId,
          gmailMessageSnippet = snippet,
          gmailMessageSizeEstimate = sizeEstimate,
          gmailMessageHistoryId = historyId,
          gmailMessageInternalDate = internalDate,
          gmailMessageMimeType = mimeType
        }

instance ToJSON GmailMessage where
  toJSON GmailMessage {gmailMessageThreadId, gmailMessageSnippet, gmailMessageSizeEstimate, gmailMessageHistoryId, gmailMessageInternalDate, gmailMessageMimeType} =
    object
      [ "threadId" .= gmailMessageThreadId,
        "snippet" .= gmailMessageSnippet,
        "sizeEstimate" .= gmailMessageSizeEstimate,
        "historyId" .= gmailMessageHistoryId,
        "internalDate" .= gmailMessageInternalDate,
        "mimeType" .= gmailMessageMimeType
      ]
