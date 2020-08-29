module Gmail where

import Control.Exception
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import GoogleAuth
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API hiding (addHeader)
import Servant.Client
import Servant.Client.Core (AuthClientData, AuthenticatedRequest, Request, addHeader, mkAuthenticatedRequest)

gmailAPI :: Proxy GmailAPI
gmailAPI = Proxy

-- authentication of token type will be a string
type instance AuthClientData (AuthProtect "token") = String

authenticateReq :: String -> Request -> Request
authenticateReq s req = addHeader "Authorization" s req

type GmailAPI =
  "gmail" :> "v1" :> "users" :> "me" :> "messages" :> AuthProtect "token" :> Get '[JSON] GmailMessageList

-- labelIds=UNREAD
listMessages :: IO (Either String [GmailMessage])
listMessages = do
  token <- getToken
  -- let token = "ya29.a0AfH6SMAY5N0D9zk4WkbsZfw6Nc11msk1xYNQo42L2R39zawN0It0vfTyMovZu3GvUem23vhqhEAs3YF4etFwu3zrxWDqUH3r8oN94UBTuSbKPneGRj8Id6E0Q7sAGkTrWH3DoDbcMSe61f7UGrO1Z1TmWxz3Fc8ZGpaLcQ"
  gmailMessageListResponse <- callGmailMessageListApi ("Bearer " ++ token)
  case gmailMessageListResponse of
    Left error -> do
      -- TODO we must return a string for Left
      putStrLn ("Bad Gmail API response: " ++ error)
      throw (GmailGetMessageListException error)
    Right gmailMessageList -> pure $ Right gmailMessageList

callGmailMessageListApi :: String -> IO (Either String [GmailMessage])
callGmailMessageListApi token = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (getGmailMessageList (mkAuthenticatedRequest token authenticateReq)) (mkClientEnv manager' (BaseUrl Https "www.googleapis.com" 443 ""))
  pure $ case res of
    Left error -> Left $ show error
    -- Use constructor using the argument order
    -- Right (GmailMessageList gmailMessageList) -> Right gmailMessageList
    -- Use constructor using the argument name = more safe
    Right GmailMessageList {messages = gmailMessageList} -> Right gmailMessageList

getGmailMessageList :: AuthenticatedRequest (AuthProtect "token") -> ClientM GmailMessageList
getGmailMessageList = client gmailAPI

data GmailMessage = GmailMessage
  { id :: String,
    threadId :: String
  }
  deriving (Generic)

instance FromJSON GmailMessage

newtype GmailMessageList = GmailMessageList
  { messages :: [GmailMessage]
  }
  deriving (Generic)

instance FromJSON GmailMessageList

data GmailException = GmailGetMessageListException String | GmailGetMessageException String
  deriving (Show)

instance Exception GmailException

-- exemple: https://gist.github.com/krisis/891d0f9984a491e471142f1829785d71
