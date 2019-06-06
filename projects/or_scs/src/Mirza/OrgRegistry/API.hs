{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Mirza.OrgRegistry.API
  ( serverAPI
  , ServerAPI
  , PublicAPI
  , PrivateAPI
  , ProtectedAPI
  , API, api
  ) where

import           Mirza.Common.Time                 (ExpirationTime,
                                                    RevocationTime)
import           Mirza.Common.Types                (ORKeyId)
import           Mirza.OrgRegistry.Database.Schema (LocationId)
import           Mirza.OrgRegistry.Types           as ST

import           Data.GS1.EPC                      as EPC

import           Servant
import           Servant.API.Flatten
import           Servant.Auth.Server
import           Servant.Swagger.UI

import           Crypto.JOSE.JWK
import           Data.Text                         (Text)
import           Data.Time                         (UTCTime)


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
      :> Capture "keyId" ORKeyId
      :> Get '[JSON] KeyInfoResponse
  :<|> "keys"
      :> Capture "keyId" ORKeyId
      :> "jwk"
      :> Get '[JSON] JWK
  :<|> "orgs"
      :> QueryParam "gs1companyprefix" GS1CompanyPrefix
      :> QueryParam "name" Text
      :> QueryParam "modifiedsince" UTCTime
      :> Get '[JSON] [OrgResponse]
  :<|> "locations"
      :> Capture "GLN" EPC.LocationEPC
      :> Get  '[JSON] LocationResponse
  :<|> "locations"
      :> QueryParam "gs1companyprefix" GS1CompanyPrefix
      :> QueryParam "modifiedsince" UTCTime
      :> Get '[JSON] [LocationResponse]
  :<|> "prototype"
      :> "orglocations"
      :> QueryParams "gs1companyprefix" GS1CompanyPrefix
      :> Get '[JSON] [OrgAndLocationResponse]
  :<|> "prototype"
      :> "orglocations"
      :> Capture "GLN" EPC.LocationEPC
      :> Capture "gs1companyprefix" GS1CompanyPrefix
      :> Get '[JSON] OrgAndLocationResponse

type PrivateAPI =
       "user"          :> Put '[JSON] NoContent
  :<|> "user"
      :> "orgs"
      :> Get '[JSON] [OrgResponse]
  :<|> "orgs"
      :> Capture "gs1CompanyPrefix" GS1CompanyPrefix
      :> ReqBody '[JSON] PartialNewOrg
      :> Put '[JSON] NoContent
  :<|> "orgs"
      :> Capture "gs1CompanyPrefix" GS1CompanyPrefix
      :> "member"
      :> Capture "userId" UserId
      :> Put '[JSON] NoContent
  :<|> "keys"
      :> ReqBody '[JSON] JWK
      :> QueryParam "expirationTime" ExpirationTime
      :> Post '[JSON] ORKeyId
  :<|> "keys"
      :> Capture "keyId" ORKeyId
      :> "revoke"
      :> Post '[JSON] RevocationTime
  :<|> "locations"
      :> ReqBody '[JSON] NewLocation
      :> Post '[JSON] LocationId
