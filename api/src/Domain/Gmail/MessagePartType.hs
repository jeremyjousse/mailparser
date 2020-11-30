module Domain.Gmail.MessagePartType where

import Adapter.HTTP.Client.GoogleAuth
import Control.Exception
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core (AuthClientData, AuthenticatedRequest, Request, addHeader, mkAuthenticatedRequest)

data GmailMessagePart = GmailMessagePart
  { partId :: String,
    mimeType :: String,
    filename :: String,
    headers :: [GmailMessagePartHeader],
    body :: GmailMessagePartBody
  }
  deriving (Generic, Show)

instance FromJSON GmailMessagePart

data GmailMessagePartHeader = GmailMessagePartHeader
  { name :: String,
    value :: String
  }
  deriving (Generic, Show)

instance FromJSON GmailMessagePartHeader

data GmailMessagePartBody = GmailMessagePartBody
  { size :: Int,
    maybeData :: Maybe String
  }
  deriving (Generic, Show)

instance FromJSON GmailMessagePartBody
