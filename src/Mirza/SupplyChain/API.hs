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

module Mirza.SupplyChain.API where

import qualified Mirza.SupplyChain.StorageBeam as SB

import           Mirza.SupplyChain.Types       as ST

import qualified Data.GS1.Event                as Ev
import           Data.GS1.EventId

import           Data.Time.Clock               (UTCTime)

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
type ProtectedAPI = Flat (BasicAuth "foo-realm" User :> PrivateAPI)

serverAPI :: Proxy ServerAPI
serverAPI = Proxy


type PublicAPI =
  -- Users
         "newUser"                            :> ReqBody '[JSON] NewUser                                        :> Post '[JSON] UserID
  -- Business
    :<|> "key"      :> "get"                  :> Capture "keyID" KeyID                                          :> Get '[JSON] PEM_RSAPubKey
    :<|> "key"      :> "getInfo"              :> Capture "keyID" KeyID                                          :> Get '[JSON] KeyInfo
    :<|> "business" :> "list"                                                                                   :> Get '[JSON] [Business]


type PrivateAPI =
-- Contacts
       "contacts"                                                                                             :> Get '[JSON] [User]
  :<|> "contacts" :> "add"                  :> Capture "userID" ST.UserID                                     :> Get '[JSON] Bool
  :<|> "contacts" :> "remove"               :> Capture "userID" ST.UserID                                     :> Get '[JSON] Bool
  :<|> "contacts" :> "search"               :> Capture "term" String                                          :> Get '[JSON] [User]
-- Signatures
  :<|> "event"    :> "addUser"              :> Capture "userID" ST.UserID       :> Capture "eventID" EventId  :> Post '[JSON] ()
  :<|> "event"    :> "sign"                 :> ReqBody '[JSON] SignedEvent                                    :> Post '[JSON] SB.PrimaryKeyType
  :<|> "event"    :> "getHash"              :> ReqBody '[JSON] EventId                                        :> Post '[JSON] HashedEvent
-- Queries
  :<|> "epc"                                :> Capture "urn" ST.LabelEPCUrn     :> "info"                     :> Get '[JSON] EPCState
  :<|> "epc"                                :> Capture "urn" ST.LabelEPCUrn     :> "events"                   :> Get '[JSON] [Ev.Event]
  :<|> "event"                              :> Capture "eventID" EventId        :> "info"                     :> Get '[JSON] (Maybe Ev.Event)
  :<|> "event"    :> "list"                 :> Capture "userID" ST.UserID                                     :> Get '[JSON] [Ev.Event]
  :<|> "event"    :> "listUsers"            :> Capture "eventID" EventId                                      :> Get '[JSON] [(User, Bool)]
-- Event Registration
  :<|> "event"    :> "objectEvent"          :> ReqBody '[JSON] ObjectEvent                                    :> Post '[JSON] (Ev.Event, SB.EventId)
  :<|> "event"    :> "aggregateEvent"       :> ReqBody '[JSON] AggregationEvent                               :> Post '[JSON] (Ev.Event, SB.EventId)
  :<|> "event"    :> "transactionEvent"     :> ReqBody '[JSON] TransactionEvent                               :> Post '[JSON] (Ev.Event, SB.EventId)
  :<|> "event"    :> "transformationEvent"  :> ReqBody '[JSON] TransformationEvent                            :> Post '[JSON] (Ev.Event, SB.EventId)
-- Business
  :<|> "key"      :> "add" :> ReqBody '[JSON] PEM_RSAPubKey :> QueryParam "expirationTime" ExpirationTime :> Post '[JSON] KeyID
  :<|> "key"      :> "revoke"              :> Capture "keyID" KeyID               :> Post '[JSON] UTCTime
