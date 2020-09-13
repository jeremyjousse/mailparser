module Api where

import Data.Proxy
import Data.Text

import Database.Persist

import Effects.Database.Types

import Servant.API

type Api =
       "brand" :> ReqBody '[JSON] Brand :> Post '[JSON] (Maybe (Key Brand))
  :<|> "brand" :> Capture "id" (Key Brand) :> Get  '[JSON] (Maybe Brand)
  :<|> "brand" :> Capture "id" (Key Brand) :> Delete '[JSON] Text
  :<|> "brand" :> Capture "id" (Key Brand) :> ReqBody '[JSON] Brand :> Put '[JSON] Text
  :<|> "gmail" :> "messages" :> "update" :> Post '[JSON] Text

api :: Proxy Api
api = Proxy
