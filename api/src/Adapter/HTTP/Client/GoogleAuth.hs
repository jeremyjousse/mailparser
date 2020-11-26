module Adapter.HTTP.Client.GoogleAuth where

import Control.Exception
import Data.Aeson
import Data.Either (lefts)
import Data.Proxy
import Data.Text (Text)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import System.Environment (lookupEnv)
import Web.FormUrlEncoded

getToken :: IO String
getToken = do
  eitherGoogleAuthClientSecret <- checkEnvVar "GOOGLE_AUTH_CLIENT_SECRET"
  eitherGoogleAuthClientId <- checkEnvVar "GOOGLE_AUTH_CLIENT_ID"
  eitherGoogleAuthCode <- checkEnvVar "GOOGLE_AUTH_CODE"
  eitherGoogleAuthRefreshToken <- checkEnvVar "GOOGLE_AUTH_REFRESH_TOKEN"

  case (eitherGoogleAuthClientId, eitherGoogleAuthClientSecret, eitherGoogleAuthRefreshToken) of
    (Right googleAuthClientId, Right googleAuthClientSecret, Right googleAuthRefreshToken) -> do
      googleAuthResponse <- callGoogleAuthApi googleAuthClientId googleAuthClientSecret googleAuthRefreshToken
      case googleAuthResponse of
        Left error -> do
          throw (GoogleAuthGetTokenException error)
        Right googleAuthToken -> pure googleAuthToken
    _ -> do
      let errors = lefts [eitherGoogleAuthClientId, eitherGoogleAuthClientSecret, eitherGoogleAuthRefreshToken]
      throw (GoogleAuthEnvVarException errors)

checkEnvVar :: String -> IO (Either String String)
checkEnvVar var = do
  maybeMyEnvVar <- lookupEnv var
  pure $ case maybeMyEnvVar of
    Nothing -> Left var
    Just myEnvVar -> Right myEnvVar

callGoogleAuthApi :: String -> String -> String -> IO (Either String String)
callGoogleAuthApi googleAuthClientId googleAuthClientSecret googleAuthRefreshToken = do
  let googleAuthRequest =
        GoogleAuthRequest
          { grantType = "refresh_token",
            clientId = googleAuthClientId,
            clientSecret = googleAuthClientSecret,
            refreshToken = googleAuthRefreshToken,
            redirectUri = "urn:ietf:wg:oauth:2.0:oob"
          }

  manager' <- newManager tlsManagerSettings
  res <- runClientM (postToken googleAuthRequest) (mkClientEnv manager' (BaseUrl Https "oauth2.googleapis.com" 443 ""))
  pure $ case res of
    Left error -> Left $ show error
    Right response -> Right $ accessToken response

googleAuthAPI :: Proxy GoogleAuthAPI
googleAuthAPI = Proxy

postToken :: GoogleAuthRequest -> ClientM GoogleAuthResponse
postToken = client googleAuthAPI

data GoogleAuthToken = GoogleAuthToken
  { jwt :: String,
    expirDateTime :: String
  }

data GoogleAuthResponse = GoogleAuthResponse
  { accessToken :: String,
    tokenType :: String,
    expiresIn :: Int,
    scope :: String
  }

instance FromJSON GoogleAuthResponse where
  parseJSON = withObject "GoogleAuthResponse" $ \googleAuthResponseJson -> do
    accessToken <- googleAuthResponseJson .: "access_token"
    tokenType <- googleAuthResponseJson .: "token_type"
    expiresIn <- googleAuthResponseJson .: "expires_in"
    scope <- googleAuthResponseJson .: "scope"
    pure
      GoogleAuthResponse
        { accessToken = accessToken,
          tokenType = tokenType,
          expiresIn = expiresIn,
          scope = scope
        }

data GoogleAuthRequest = GoogleAuthRequest
  { grantType :: String,
    clientId :: String,
    clientSecret :: String,
    refreshToken :: String,
    redirectUri :: String
  }

instance ToForm GoogleAuthRequest where
  toForm googleAuthRequest =
    [ ("grant_type", toQueryParam (grantType googleAuthRequest)),
      ("client_id", toQueryParam (clientId googleAuthRequest)),
      ("client_secret", toQueryParam (clientSecret googleAuthRequest)),
      ("refresh_token", toQueryParam (refreshToken googleAuthRequest)),
      ("redirect_url", toQueryParam (redirectUri googleAuthRequest))
    ]

data GoogleAuthException = GoogleAuthEnvVarException [String] | GoogleAuthGetTokenException String
  deriving (Show)

instance Exception GoogleAuthException

type GoogleAuthAPI =
  "token" :> ReqBody '[FormUrlEncoded] GoogleAuthRequest :> Post '[JSON] GoogleAuthResponse
