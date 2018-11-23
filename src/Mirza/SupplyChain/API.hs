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
  (
    serverAPI
  , ServerAPI
  , PublicAPI
  , PrivateAPI
  , ProtectedAPI
  , API, api
  ) where

import qualified Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.Types           as ST

import           Data.GS1.EPC                      (GS1CompanyPrefix)
import qualified Data.GS1.Event                    as Ev
import           Data.GS1.EventId                  as EvId

import           Servant
import           Servant.API.Flatten
import           Servant.Swagger.UI

import           Data.Text                         (Text)

type API
    -- this serves both: swagger.json and swagger-ui
    = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI

api :: Proxy API
api = Proxy


type ServerAPI = PublicAPI :<|> ProtectedAPI
type ProtectedAPI = Flat (BasicAuth "foo-realm" User :> PrivateAPI)

serverAPI :: Proxy ServerAPI
serverAPI = Proxy


type PublicAPI =
  -- Health
       "healthz" :> Get '[JSON] HealthResponse
  -- Users
  :<|> "newUser"                            :> ReqBody '[JSON] NewUser                                      :> Post '[JSON] UserId

type PrivateAPI =
-- Contacts
       "contacts"                                                                                             :> Get '[JSON] [User]
  :<|> "contacts" :> "add"                  :> Capture "userId" ST.UserId                                     :> Get '[JSON] Bool
  :<|> "contacts" :> "remove"               :> Capture "userId" ST.UserId                                     :> Get '[JSON] Bool
-- Users
  :<|> "user"     :> "search"               :> QueryParam "GS1CompanyPrefix" GS1CompanyPrefix
                                            :> QueryParam "lastname" Text
                                            :> Post '[JSON] [User]
-- Signatures
  :<|> "event"    :> "sign"                 :> ReqBody '[JSON] SignedEvent                                    :> Post '[JSON] EventInfo
  :<|> "event"    :> "getHash"              :> ReqBody '[JSON] EventId                                        :> Post '[JSON] HashedEvent
-- Queries
  :<|> "epc"      :> "events"               :> Capture "urn" ST.LabelEPCUrn                                   :> Get '[JSON] [Ev.Event]
  :<|> "event"    :> "info"                 :> Capture "eventId" EventId                                      :> Get '[JSON] EventInfo
  :<|> "event"    :> "list"                 :> Capture "userId" UserId                                        :> Get '[JSON] [Ev.Event]
  :<|> "event"    :> "listUsers"            :> Capture "eventId" EventId                                      :> Get '[JSON] [(User, Bool)]
  :<|> "user"     :> "getId"                                                                                  :> Get '[JSON] UserId
-- Event Registration
  :<|> "event"    :> "objectEvent"          :> ReqBody '[JSON] ObjectEvent                                    :> Post '[JSON] (EventInfo, Schema.EventId)
  :<|> "event"    :> "aggregateEvent"       :> ReqBody '[JSON] AggregationEvent                               :> Post '[JSON] (EventInfo, Schema.EventId)
  :<|> "event"    :> "transactionEvent"     :> ReqBody '[JSON] TransactionEvent                               :> Post '[JSON] (EventInfo, Schema.EventId)
  :<|> "event"    :> "transformationEvent"  :> ReqBody '[JSON] TransformationEvent                            :> Post '[JSON] (EventInfo, Schema.EventId)
