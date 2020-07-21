module GoogleAuth where

import           System.Environment             ( lookupEnv )
import           Data.Either                    ( lefts )
import           Control.Exception
import           Data.Text                      ( Text )
import           Web.FormUrlEncoded
import           Servant.API


getToken :: IO String
getToken = do
    googleAuthClientSecret <- checkEnvVar "GOOGLE_AUTH_CLIENT_SECRET"
    googleAuthClientId     <- checkEnvVar "GOOGLE_AUTH_CLIENT_ID"
    googleAuthCode         <- checkEnvVar "GOOGLE_AUTH_CODE"

    let errors =
            lefts [googleAuthClientId, googleAuthClientSecret, googleAuthCode]

    if length errors /= 0
        then do
            putStrLn ("Missing env vars: " ++ show errors)
            throw (GoogleAuthEnvVarException errors)
        else undefined


checkEnvVar :: String -> IO (Either String String)
checkEnvVar var = undefined
-- lookupEnv

callGoogleAuthApi :: String -> String -> String -> IO (Either String String)
callGoogleAuthApi googleAuthClientId googleAuthClientSecret googleAuthCode =
    undefined

data GoogleAuthToken = GoogleAuthToken {
    jwt :: String,
    expirDateTime :: String
}

data GoogleAuthResponse = GoogleAuthResponse {accessToken :: Text,
                                  tokenType :: Text,
                                  expiresIn :: Int,
                                  refreshToken :: Text}

data GoogleAuthRequest = GoogleAuthRequest {grantType :: Text,
                                  code :: Text,
                                  clientId :: Text,
                                  clientSecret :: Text,
                                  redirectUri :: Text}

instance ToForm GoogleAuthRequest where 
    toForm googleAuthRequest =
        [ ("grant_ype", toQueryParam (grantType googleAuthRequest))
        , ("code", toQueryParam (code googleAuthRequest)) 
        , ("client_id", toQueryParam (clientId googleAuthRequest))
        , ("client_secret", toQueryParam (clientSecret googleAuthRequest))
        , ("redirect_url", toQueryParam (redirectUri googleAuthRequest)) ]

data GoogleAuthEnvVarException = GoogleAuthEnvVarException [String]
    deriving Show

instance Exception GoogleAuthEnvVarException

type GoogleAuthAPI = 
    "token" :> ReqBody '[FormUrlEncoded] GoogleAuthRequest :> Post '[JSON] (Maybe (GoogleAuthResponse))