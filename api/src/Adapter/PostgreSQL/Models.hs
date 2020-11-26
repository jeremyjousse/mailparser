{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- module Effects.Database.Types where
module Adapter.PostgreSQL.Models where

import Data.Aeson
import Data.Text
import Database.Persist ()
import Database.Persist.Sql
  ( Migration,
    addMigration,
  )
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Brand
      name Text
      url Text
      deriving Show
  EmailMessage
      threadId Text
      snippet Text
      sizeEstimate Int
      historyId Text
      internalDate Text
      mimeType Text
      deriving Show
  EmailMessageHeader
      name Text
      value Text
      emailMessage EmailMessageId
  EmailMessagePart
      partId Int
      data Text
      mimeType Text
      emailMessage EmailMessageId
  Sender
      name Text
      email Text
      mxRecord Text Maybe
      brandId BrandId Maybe
      UniqueEmail email
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

instance FromJSON EmailMessage where
  parseJSON = withObject "EmailMessage" $ \emailMessageJson -> do
    threadId <- emailMessageJson .: "threadId"
    snippet <- emailMessageJson .: "snippet"
    sizeEstimate <- emailMessageJson .: "sizeEstimate"
    historyId <- emailMessageJson .: "historyId"
    internalDate <- emailMessageJson .: "internalDate"
    mimeType <- emailMessageJson .: "mimeType"
    pure
      EmailMessage
        { emailMessageThreadId = threadId,
          emailMessageSnippet = snippet,
          emailMessageSizeEstimate = sizeEstimate,
          emailMessageHistoryId = historyId,
          emailMessageInternalDate = internalDate,
          emailMessageMimeType = mimeType
        }

instance ToJSON EmailMessage where
  toJSON EmailMessage {emailMessageThreadId, emailMessageSnippet, emailMessageSizeEstimate, emailMessageHistoryId, emailMessageInternalDate, emailMessageMimeType} =
    object
      [ "threadId" .= emailMessageThreadId,
        "snippet" .= emailMessageSnippet,
        "sizeEstimate" .= emailMessageSizeEstimate,
        "historyId" .= emailMessageHistoryId,
        "internalDate" .= emailMessageInternalDate,
        "mimeType" .= emailMessageMimeType
      ]
