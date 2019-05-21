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
import           Servant.Auth.Server

import           Crypto.JOSE.JWK
import           Data.Text                              (Text)
import           Data.Time                              (UTCTime)


type API
    -- this serves both: swagger.json and swagger-ui
    = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI

api :: Proxy API
api = Proxy


type ServerAPI = PublicAPI :<|> ProtectedAPI
type ProtectedAPI = Flat (Servant.Auth.Server.Auth '[JWT] VerifiedTokenClaims :> PrivateAPI)

serverAPI :: Proxy ServerAPI
serverAPI = Proxy


type PublicAPI =
       "healthz" :> Get '[JSON] HealthResponse
  :<|> "version" :> Get '[JSON] String
  :<|> "keys"
      :> Capture "keyId" BRKeyId
      :> Get '[JSON] KeyInfoResponse
  :<|> "keys"
      :> Capture "keyId" BRKeyId
      :> "jwk"
      :> Get '[JSON] JWK
  :<|> "organisations"
      :> QueryParam "gs1companyprefix" GS1CompanyPrefix
      :> QueryParam "name" Text
      :> QueryParam "modifiedsince" UTCTime
      :> Get '[JSON] [BusinessResponse]
  :<|> "locations"
      :> Capture "GLN" EPC.LocationEPC
      :> Get  '[JSON] LocationResponse
  :<|> "locations"
      :> QueryParam "gs1companyprefix" GS1CompanyPrefix
      :> QueryParam "modifiedsince" UTCTime
      :> Get '[JSON] [LocationResponse]
  :<|> "prototype"
      :> "businesslocations"
      :> QueryParams "gs1companyprefix" GS1CompanyPrefix
      :> Get '[JSON] [BusinessAndLocationResponse]
  :<|> "prototype"
      :> "businesslocations"
      :> Capture "GLN" EPC.LocationEPC
      :> Capture "gs1companyprefix" GS1CompanyPrefix
      :> Get '[JSON] BusinessAndLocationResponse

type PrivateAPI =
       "user"          :> Put '[JSON] NoContent
  :<|> "user"
      :> "organisations"
      :> Get '[JSON] [BusinessResponse]
  :<|> "organisations"
      :> Capture "gs1CompanyPrefix" GS1CompanyPrefix
      :> ReqBody '[JSON] PartialNewBusiness
      :> Put '[JSON] NoContent
  :<|> "organisations"
      :> Capture "gs1CompanyPrefix" GS1CompanyPrefix
      :> "member"
      :> Capture "userId" UserId
      :> Put '[JSON] NoContent
  :<|> "keys"
      :> ReqBody '[JSON] JWK
      :> QueryParam "expirationTime" ExpirationTime
      :> Post '[JSON] BRKeyId
  :<|> "keys"
      :> Capture "keyId" BRKeyId
      :> "revoke"
      :> Post '[JSON] RevocationTime
  :<|> "locations"
      :> ReqBody '[JSON] NewLocation
      :> Post '[JSON] LocationId
