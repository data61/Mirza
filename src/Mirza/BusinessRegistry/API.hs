{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Mirza.BusinessRegistry.API
  (
    serverAPI
  , ServerAPI
  , PublicAPI
  , PrivateAPI
  , ProtectedAPI
  , API, api
  ) where

import           Mirza.BusinessRegistry.Types as ST
import           Mirza.Common.Time            (ExpirationTime, RevocationTime)
import           Mirza.Common.Types           (KeyID)

import           Servant
import           Servant.API.Flatten
import           Servant.Swagger.UI

type API
    -- this serves both: swagger.json and swagger-ui
    = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI

api :: Proxy API
api = Proxy


type ServerAPI = PublicAPI :<|> ProtectedAPI
type ProtectedAPI = Flat (BasicAuth "foo-realm" AuthUser :> PrivateAPI)

serverAPI :: Proxy ServerAPI
serverAPI = Proxy


type PublicAPI =
  -- Business
         "key"      :> "get"                  :> Capture "keyID" KeyID                                          :> Get '[JSON] PEM_RSAPubKey
    :<|> "key"      :> "getInfo"              :> Capture "keyID" KeyID                                          :> Get '[JSON] KeyInfoResponse
    :<|> "business" :> "list"                                                                                   :> Get '[JSON] [BusinessResponse]


type PrivateAPI =
-- Business
       "key"      :> "add" :> ReqBody '[JSON] PEM_RSAPubKey :> QueryParam "expirationTime" ExpirationTime :> Post '[JSON] KeyID
  :<|> "key"      :> "revoke"              :> Capture "keyID" KeyID               :> Post '[JSON] RevocationTime
