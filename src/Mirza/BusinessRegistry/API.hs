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

import           Mirza.BusinessRegistry.Database.Schema (LocationId)
import           Mirza.BusinessRegistry.Types           as ST
import           Mirza.Common.Time                      (ExpirationTime,
                                                         RevocationTime)
import           Mirza.Common.Types                     (BRKeyId)

import           Data.GS1.EPC                           as EPC

import           Servant
import           Servant.API.Flatten
import           Servant.Swagger.UI

import           Crypto.JOSE.JWK
import           Data.Text                              (Text)
import           Data.Time                              (UTCTime)


import Servant.Auth.Server

type API
    -- this serves both: swagger.json and swagger-ui
    = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI

api :: Proxy API
api = Proxy


type ServerAPI = PublicAPI :<|> ProtectedAPI
type ProtectedAPI = Flat ((Servant.Auth.Server.Auth '[JWT]) AuthUser :> PrivateAPI)

serverAPI :: Proxy ServerAPI
serverAPI = Proxy


type PublicAPI =
       "healthz"                                          :> Get '[JSON] HealthResponse
  :<|> "version"                                          :> Get '[JSON] String
  :<|> "key"      :> "get"     :> Capture "keyId" BRKeyId :> Get '[JSON] JWK
  :<|> "key"      :> "getInfo" :> Capture "keyId" BRKeyId :> Get '[JSON] KeyInfoResponse
  :<|> "business" :> "search"
      :> QueryParam "gs1id" GS1CompanyPrefix
      :> QueryParam "name" Text
      :> QueryParam "modifiedsince" UTCTime
      :> Get '[JSON] [BusinessResponse]
  :<|> "location"  :> "get"      :> Capture "GLN" EPC.LocationEPC :> Get  '[JSON] LocationResponse
  :<|> "location"  :> "search"
      :> QueryParam "gs1id" GS1CompanyPrefix
      :> QueryParam "modifiedsince" UTCTime
      :> Get '[JSON] [LocationResponse]
  :<|> "prototype" :> "location" :> "ux" :> QueryParams "gs1companyprefix" GS1CompanyPrefix :> Get '[JSON] [BusinessAndLocationResponse]
  :<|> "prototype" :> "location" :> "uxgln"
          :> Capture "GLN" EPC.LocationEPC
          :> Capture "gs1companyprefix" GS1CompanyPrefix
          :> Get '[JSON] BusinessAndLocationResponse


type PrivateAPI =
       "user"      :> "add"      :> ReqBody '[JSON] NewUser     :> Post '[JSON] UserId
  :<|> "business"  :> "add"      :> ReqBody '[JSON] NewBusiness :> Post '[JSON] GS1CompanyPrefix
