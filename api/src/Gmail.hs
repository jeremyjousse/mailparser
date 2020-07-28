module Gmail where

import           Control.Exception
import           Data.Text                      ( Text )
import           Data.Proxy
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Network.HTTP.Client (newManager)
import           GoogleAuth
import           Servant.API  hiding (addHeader)
import           Servant.Client
import           Servant.Client.Core (AuthClientData, AuthenticatedRequest, Request, addHeader, mkAuthenticatedRequest)

gmailAPI :: Proxy GmailAPI
gmailAPI = Proxy

type instance AuthClientData (AuthProtect "token") = String

authenticateReq :: String -> Request -> Request
authenticateReq s req = addHeader "token" s req

type GmailAPI = 
    "gmail" :> "v1" :> "users" :> "me" :> "messages" :> AuthProtect "token" :> Post '[JSON] [GmailMessageInListe]

listMessages :: IO (Either String [GmailMessageInListe])
listMessages = do
    -- let token = getToken
    let token = "ya29.a0AfH6SMBIjv_XvU62IGa4WgBnytWpeE7ParKpia1kVpj3srIEeADYWcfQ44_SPv_ZOOkPpSjkoLmT3dnCFiZu241887G_HdFk0D6X0cWU_WjeBlufzeDbwf7NLaG_xddrLG6bEDRoOU773c_TCNcG9qt75S-vWegnmKcO"
    gmailMessageListResponse <- callGmailMessageListApi token
    case gmailMessageListResponse of
        Left error -> do
            putStrLn ("Bad Gmail API response: " ++ error)
            throw (GmailGetMessageListException error)
        Right gmailMessageList -> pure gmailMessageList

callGmailMessageListApi :: IO String -> IO Either(String [GmailMessageInListe])
callGmailMessageListApi token = do
    manager' <- newManager tlsManagerSettings
    res <- runClientM (getGmailMessageList (mkAuthenticatedRequest token authenticateReq)) (mkClientEnv manager' (BaseUrl Https "www.googleapis.com" 443 ""))
    pure $ case res of
            Left error -> Left $ show error
            Right response -> Right $ response

getGmailMessageList :: AuthenticatedRequest (AuthProtect "token") -> ClientM [GmailMessageInListe]
getGmailMessageList = client gmailAPI

data GmailMessageInListe = GmailMessageInListe {
    id :: String,
    threadId :: String
}

data GmailException = GmailGetMessageListException String |GmailGetMessageException String
    deriving Show

-- exemple: https://gist.github.com/krisis/891d0f9984a491e471142f1829785d71
