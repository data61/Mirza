{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Mirza.Blockchain.API
  (
    serverAPI
  , ServerAPI
  , PublicAPI
  , API, api
  ) where

import           Mirza.Blockchain.Types as BcT

import           Servant
import           Servant.Swagger.UI

type API
    -- this serves both: swagger.json and swagger-ui
    = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI

api :: Proxy API
api = Proxy


type ServerAPI = PublicAPI

serverAPI :: Proxy ServerAPI
serverAPI = Proxy


type PublicAPI =
-- Events
       "events"                :> ReqBody '[JSON] EventHash :> ReqBody '[JSON] IdentifierHash   :> Post '[JSON] EventInsertionResponse
  :<|> "events"                :> ReqBody '[JSON] IdentifierHash                                :> Get '[JSON] [EventInsertionResponse]
  :<|> "events" :> "eventhash" :> Capture "eventHash" EventHash                                 :> Get '[JSON] EventInsertionResponse
  :<|> "events" :> "txhash"    :> Capture "blockchainTransactionHash" BlockchainTransactionHash :> Get '[JSON] EventInsertionResponse
  -- Health
  :<|> "health" :> Get '[JSON] HealthResponse
