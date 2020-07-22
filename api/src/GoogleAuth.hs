module GoogleAuth where

import           System.Environment             ( lookupEnv )
import           Data.Either                    ( lefts )
import           Control.Exception
import           Data.Text                      ( Text )
import           Data.Proxy
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.HTTP.Client (newManager)
import           Data.Aeson
import           Web.FormUrlEncoded
import           Servant.API
import           Servant.Client


getToken :: IO String
getToken = do
    eitherGoogleAuthClientSecret <- checkEnvVar "GOOGLE_AUTH_CLIENT_SECRET"
    eitherGoogleAuthClientId     <- checkEnvVar "GOOGLE_AUTH_CLIENT_ID"
    eitherGoogleAuthCode         <- checkEnvVar "GOOGLE_AUTH_CODE"

    case (eitherGoogleAuthClientId, eitherGoogleAuthClientSecret, eitherGoogleAuthCode) of
        (Right googleAuthClientId,  Right googleAuthClientSecret, Right googleAuthCode) -> do
            googleAuthResponse <- callGoogleAuthApi googleAuthClientId googleAuthClientSecret googleAuthCode
            case googleAuthResponse of
                Left error -> do
                    putStrLn ("Bad google token response: " ++ error)
                    throw ( GoogleAuthGetTokenException error)
                Right googleAuthToken -> pure googleAuthToken
        _ -> do
            let errors = lefts [eitherGoogleAuthClientId, eitherGoogleAuthClientSecret, eitherGoogleAuthCode]
            putStrLn ("Missing env vars: " ++ show errors)
            throw (GoogleAuthEnvVarException errors)

checkEnvVar :: String -> IO (Either String String)
checkEnvVar var = do
    maybeMyEnvVar <- lookupEnv var
    pure $ case maybeMyEnvVar of
            Nothing -> Left var
            Just myEnvVar -> Right myEnvVar


callGoogleAuthApi :: String -> String -> String -> IO (Either String String)
callGoogleAuthApi googleAuthClientId googleAuthClientSecret googleAuthCode = do
    let googleAuthRequest = GoogleAuthRequest { grantType = "authorization_code"
    , code = googleAuthCode
    , clientId = googleAuthClientId
    , clientSecret = googleAuthClientSecret
    , redirectUri = "urn:ietf:wg:oauth:2.0:oob" }

    manager' <- newManager tlsManagerSettings
    res <- runClientM (postToken googleAuthRequest) (mkClientEnv manager' (BaseUrl Https "oauth2.googleapis.com" 443 ""))
    pure $ case res of
            Left error -> Left $ show error
            Right response -> Right $ accessToken response

googleAuthAPI :: Proxy GoogleAuthAPI
googleAuthAPI = Proxy

postToken :: GoogleAuthRequest -> ClientM GoogleAuthResponse
postToken = client googleAuthAPI

data GoogleAuthToken = GoogleAuthToken {
    jwt :: String,
    expirDateTime :: String
}

data GoogleAuthResponse = GoogleAuthResponse {accessToken :: String,
                                  tokenType :: String,
                                  expiresIn :: Int,
                                  refreshToken :: String}

instance FromJSON GoogleAuthResponse where
    parseJSON = withObject "GoogleAuthResponse" $ \ googleAuthResponseJson -> do
            accessToken <- googleAuthResponseJson .: "access_token"
            tokenType <- googleAuthResponseJson .: "token_type"
            expiresIn <- googleAuthResponseJson .: "expires_in"
            refreshToken <- googleAuthResponseJson .: "refresh_token"
            pure GoogleAuthResponse {accessToken = accessToken
            , tokenType = tokenType
            , expiresIn = expiresIn
            , refreshToken = refreshToken
            }

data GoogleAuthRequest = GoogleAuthRequest {grantType :: String,
                                  code :: String,
                                  clientId :: String,
                                  clientSecret :: String,
                                  redirectUri :: String}

instance ToForm GoogleAuthRequest where 
    toForm googleAuthRequest =
        [ ("grant_ype", toQueryParam (grantType googleAuthRequest))
        , ("code", toQueryParam (code googleAuthRequest)) 
        , ("client_id", toQueryParam (clientId googleAuthRequest))
        , ("client_secret", toQueryParam (clientSecret googleAuthRequest))
        , ("redirect_url", toQueryParam (redirectUri googleAuthRequest)) ]

data GoogleAuthException = GoogleAuthEnvVarException [String] | GoogleAuthGetTokenException String
    deriving Show

instance Exception GoogleAuthException

type GoogleAuthAPI = 
    "token" :> ReqBody '[FormUrlEncoded] GoogleAuthRequest :> Post '[JSON] GoogleAuthResponse