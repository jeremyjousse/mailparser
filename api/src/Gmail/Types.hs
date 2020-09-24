module Gmail.Types where

import Control.Exception
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Gmail.MessagePartType
import GoogleAuth
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core (AuthClientData, AuthenticatedRequest, Request, addHeader, mkAuthenticatedRequest)

data GmailMessage = GmailMessage
  { id :: String,
    threadId :: String
  }
  deriving (Generic, Show)

instance FromJSON GmailMessage

data GmailMessageDetail = GmailMessageDetail
  { id :: String,
    threadId :: String,
    labelIds :: [String],
    snippet :: String,
    payload :: GmailMessagePayload,
    sizeEstimate :: Int,
    historyId :: String,
    internalDate :: String
  }
  deriving (Generic, Show)

instance FromJSON GmailMessageDetail

data GmailMessagePayload = GmailMessagePayload
  { partId :: String,
    mimeType :: String,
    filename :: String,
    headers :: [GmailMessageHeader],
    body :: GmailMessageBody,
    parts :: [GmailMessagePart]
  }
  deriving (Generic, Show)

instance FromJSON GmailMessagePayload

data GmailMessageHeader = GmailMessageHeader
  { name :: String,
    value :: String
  }
  deriving (Generic, Show)

instance FromJSON GmailMessageHeader

data GmailMessageBody = GmailMessageBody
  { size :: Int,
    maybeData :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON GmailMessageBody where
  parseJSON = withObject "GmailMessageBody" $ \gmailMessageBodyJson -> do
    size <- gmailMessageBodyJson .: "size"
    maybeData <- gmailMessageBodyJson .:? "data"
    pure
      GmailMessageBody
        { size = size,
          maybeData = maybeData
        }

newtype GmailMessageList = GmailMessageList
  { messages :: [GmailMessage]
  }
  deriving (Generic)

instance FromJSON GmailMessageList

data GmailException = GmailGetMessageListException String | GmailGetMessageException String
  deriving (Show)

data GmailMessageLabelUpdateRequest = GmailMessageLabelUpdateRequest
  { removeLabelIds :: [String]
  }
  deriving (Generic, Show)

instance FromJSON GmailMessageLabelUpdateRequest

instance ToJSON GmailMessageLabelUpdateRequest

data GmailMessageLabelUpdateResponse = GmailMessageLabelUpdateResponse
  { id :: String,
    threadId :: String,
    labelIds :: [String]
  }
  deriving (Generic, Show)

instance FromJSON GmailMessageLabelUpdateResponse

instance Exception GmailException
