module Adapter.HTTP.Client.Gmail (module Adapter.HTTP.Client.Gmail, module Gmail.Types) where

import Adapter.HTTP.Client.GoogleAuth
import Control.Exception
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Gmail.Types
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
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
  "gmail" :> "v1" :> "users" :> "me" :> "messages" :> QueryParam "labelIds" String :> AuthProtect "token" :> Get '[JSON] GmailMessageList
    :<|> "gmail" :> "v1" :> "users" :> "me" :> "messages" :> Capture "threadId" String :> AuthProtect "token" :> Get '[JSON] GmailMessageDetail
    :<|> "gmail" :> "v1" :> "users" :> "me" :> "messages" :> Capture "threadId" String :> "modify" :> ReqBody '[JSON] GmailMessageLabelUpdateRequest :> AuthProtect "token" :> Post '[JSON] GmailMessageLabelUpdateResponse

data GmailError = InvalidToken String | HttpConnectionError String | UnknownError String
  deriving (Show)

-- List messages
-- Insert each message to db and update message labels
-- markMessageAsRead

-- listThenUpdateMessagesInDb :: IO (Either GmailError [GmailMessage])
-- listThenUpdateMessagesInDb = do
--   messageList <- listMessages
--   updatedMessages <- callUpdateMessageLabels messageList
--   return updatedMessages
--     where callUpdateMessageLabels (Left gmailError) = []
--           callUpdateMessageLabels (Right messageList) = []

listMessages :: IO (Either GmailError [GmailMessage])
listMessages = do
  token <- getToken
  callGmailMessageListApi (Just "UNREAD") ("Bearer " ++ token)

messageDetail :: String -> IO (Either GmailError GmailMessageDetail)
messageDetail messageId = do
  token <- getToken
  callGmailMessageDetailApi messageId ("Bearer " ++ token)

updateMessageLabels :: String -> GmailMessageLabelUpdateRequest -> IO (Either GmailError GmailMessageLabelUpdateResponse)
updateMessageLabels messageId labels = do
  token <- getToken
  callGmailMessageLabelUpdateApi messageId labels ("Bearer " ++ token)

callGmailMessageDetailApi :: String -> String -> IO (Either GmailError GmailMessageDetail)
callGmailMessageDetailApi messageId token = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (getGmailMessageDetail messageId (mkAuthenticatedRequest token authenticateReq)) (mkClientEnv manager' (BaseUrl Https "www.googleapis.com" 443 ""))
  pure $ case res of
    Left err -> Left (extractError err)
    Right gmailMessageDetail -> Right gmailMessageDetail

callGmailMessageListApi :: Maybe String -> String -> IO (Either GmailError [GmailMessage])
callGmailMessageListApi label token = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (getGmailMessageList label (mkAuthenticatedRequest token authenticateReq)) (mkClientEnv manager' (BaseUrl Https "www.googleapis.com" 443 ""))
  pure $ case res of
    Left err -> Left (extractError err)
    -- Use constructor using the argument order
    -- Right (GmailMessageList gmailMessageList) -> Right gmailMessageList
    -- Use constructor using the argument name = more safe
    Right GmailMessageList {messages = gmailMessageList} -> Right gmailMessageList

callGmailMessageLabelUpdateApi :: String -> GmailMessageLabelUpdateRequest -> String -> IO (Either GmailError GmailMessageLabelUpdateResponse)
callGmailMessageLabelUpdateApi messageId labels token = do
  manager' <- newManager tlsManagerSettings
  res <- runClientM (updateGmailMessageLabels messageId labels (mkAuthenticatedRequest token authenticateReq)) (mkClientEnv manager' (BaseUrl Https "www.googleapis.com" 443 ""))
  pure $ case res of
    Left err -> Left (extractError err)
    Right gmailMessageLabelUpdateResponse -> Right gmailMessageLabelUpdateResponse

-- extractGmailResponseOrError :: Either ClientError GmailMessageList|GmailMessageDetail -> IO (Either GmailError [GmailMessage]|GmailMessageDetail)
extractError :: ClientError -> GmailError
extractError (ConnectionError error) = HttpConnectionError (show error)
extractError (FailureResponse _ (Response {responseStatusCode, responseBody})) | responseStatusCode == status401 = InvalidToken (show responseBody)
extractError clientError = UnknownError ("Unknown error" <> show clientError)

getGmailMessageList :: Maybe String -> AuthenticatedRequest (AuthProtect "token") -> ClientM GmailMessageList
getGmailMessageDetail :: String -> AuthenticatedRequest (AuthProtect "token") -> ClientM GmailMessageDetail
updateGmailMessageLabels :: String -> GmailMessageLabelUpdateRequest -> AuthenticatedRequest (AuthProtect "token") -> ClientM GmailMessageLabelUpdateResponse
getGmailMessageList :<|> getGmailMessageDetail :<|> updateGmailMessageLabels = client gmailAPI
