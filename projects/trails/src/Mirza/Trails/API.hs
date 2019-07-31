{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Mirza.Trails.API where

import           Mirza.Trails.Types (SignaturePlaceholder, TrailEntry)

import           Mirza.Common.Types (HealthResponse)

import           Data.GS1.EventId   (EventId)

import           Servant
import           Servant.Swagger.UI


type API
    -- This serves both: swagger.json and swagger-ui
    = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI


api :: Proxy API
api = Proxy


type ServerAPI = PublicAPI


serverAPI :: Proxy ServerAPI
serverAPI = Proxy


type PublicAPI =
       "healthz" :> Get '[JSON] HealthResponse
  :<|> "version" :> Get '[JSON] String
  :<|> "trail"   :> Capture "eventId"   EventId              :> Get '[JSON] [TrailEntry]
  :<|> "trail"   :> Capture "signature" SignaturePlaceholder :> Get '[JSON] [TrailEntry]
  :<|> "trail"   :> ReqBody '[JSON] [TrailEntry]             :> Post '[JSON] NoContent              -- TODO: Should fix this type so that its the correct status code.
