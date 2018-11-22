{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE DataKinds             #-}

module Mirza.Blockchain.API
  (
    serverAPI
  , ServerAPI
  , PublicAPI
  , API, api
  ) where

-- import qualified Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.Blockchain.Types  as BcT
import qualified Mirza.SupplyChain.Types as ST

import           Data.GS1.EPC            (GS1CompanyPrefix)
import qualified Data.GS1.Event          as Ev
import           Data.GS1.EventId        as EvId

import           Servant
import           Servant.API.Flatten
import           Servant.Swagger.UI

import           Data.Time               (UTCTime)

import           Data.Text               (Text)

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
-- Event Registration
       "events"                :> ReqBody '[JSON] EventHash :> ReqBody '[JSON] IdentifierHash   :> Post '[JSON] EventInsertionResponse
  :<|> "events"                :> ReqBody '[JSON] IdentifierHash                                :> Get '[JSON] [EventInsertionResponse]
  :<|> "events" :> "eventhash" :> Capture "eventHash" EventHash                                 :> Get '[JSON] EventInsertionResponse
  :<|> "events" :> "txhash"    :> Capture "blockchainTransactionHash" BlockchainTransactionHash :> Get '[JSON] EventInsertionResponse
  -- Health
  :<|> "health" :> Get '[JSON] HealthResponse
