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

module Mirza.SupplyChain.API
  ( serverAPI
  , ServerAPI
  , API, api
  ) where

import           Mirza.Common.GS1BeamOrphans        (LabelEPCUrn (..))
import qualified Mirza.SupplyChain.Database.Schema  as Schema
import           Mirza.SupplyChain.Types            as ST

import           Mirza.SupplyChain.Handlers.UXUtils (PrettyEventResponse (..))

import qualified Data.GS1.Event                     as Ev
import           Data.GS1.EventId                   as EvId

import           Servant
import           Servant.Swagger.UI

type API
    -- this serves both: swagger.json and swagger-ui
    = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI

api :: Proxy API
api = Proxy

serverAPI :: Proxy ServerAPI
serverAPI = Proxy


type ServerAPI =
  -- Health
       "healthz" :> Get '[JSON] HealthResponse
  :<|> "version" :> Get '[JSON] String

-- Signatures
  :<|> "event"    :> "sign"
                  :> ReqBody '[JSON] SignedEvent
                  :> Post '[JSON] EventInfo
-- Queries
  :<|> "epc"      :> "events"
                  :> Capture "urn" LabelEPCUrn
                  :> Get '[JSON] [Ev.Event]
  :<|> "event"    :> "info"
                  :> Capture "eventId" EventId
                  :> Get '[JSON] EventInfo
  -- :<|> "event"    :> "list"
  --                 :> Capture "userId" UserId
  --                 :> Get '[JSON] [Ev.Event]
-- Event Registration
  :<|> "event"    :> "objectEvent"
                  :> ReqBody '[JSON] ObjectEvent
                  :> Post '[JSON] (EventInfo, Schema.EventId)
  :<|> "event"    :> "aggregateEvent"
                  :> ReqBody '[JSON] AggregationEvent
                  :> Post '[JSON] (EventInfo, Schema.EventId)
  :<|> "event"    :> "transactionEvent"
                  :> ReqBody '[JSON] TransactionEvent
                  :> Post '[JSON] (EventInfo, Schema.EventId)
  :<|> "event"    :> "transformationEvent"
                  :> ReqBody '[JSON] TransformationEvent
                  :> Post '[JSON] (EventInfo, Schema.EventId)
  -- UI
  :<|> "prototype" :> "list" :> "events"
              :> Capture "urn" LabelEPCUrn
              :> Get '[JSON] [PrettyEventResponse]
