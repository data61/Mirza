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

import           Mirza.SupplyChain.Model       as M
import           Mirza.SupplyChain.StorageBeam (PrimaryKeyType)

import qualified Data.GS1.Event                as Ev
import           Data.GS1.EventID

import           Data.Swagger
import           Servant
import           Servant.API.Flatten
import           Servant.Swagger.UI

type PrivateAPI =
       "epc"      :> Capture "urn" M.LabelEPCUrn      :> "info"   :> Get '[JSON] EPCState
  :<|> "epc"      :> Capture "urn" M.LabelEPCUrn      :> "events" :> Get '[JSON] [Ev.Event]
  :<|> "event"    :> Capture "eventID" EventID        :> "info"   :> Get '[JSON] (Maybe Ev.Event)
  :<|> "contacts"                                                 :> Get '[JSON] [User]
  :<|> "contacts" :> "add"                 :> Capture "userID" M.UserID           :> Get '[JSON] Bool
  :<|> "contacts" :> "remove"              :> Capture "userID" M.UserID           :> Get '[JSON] Bool
  :<|> "contacts" :> "search"              :> Capture "term" String               :> Get '[JSON] [User]
  :<|> "event"    :> "list"                :> Capture "userID" M.UserID           :> Get '[JSON] [Ev.Event]
  :<|> "event"    :> "listUsers"           :> Capture "eventID" EventID           :> Get '[JSON] [(User, Bool)]
  :<|> "event"    :> "sign"                :> ReqBody '[JSON] SignedEvent         :> Post '[JSON] PrimaryKeyType
  :<|> "event"    :> "getHash"             :> ReqBody '[JSON] EventID             :> Post '[JSON] HashedEvent
  :<|> "event"    :> "objectEvent"         :> ReqBody '[JSON] ObjectEvent         :> Post '[JSON] Ev.Event
  :<|> "event"    :> "aggregateEvent"      :> ReqBody '[JSON] AggregationEvent    :> Post '[JSON] Ev.Event
  :<|> "event"    :> "transactionEvent"    :> ReqBody '[JSON] TransactionEvent    :> Post '[JSON] Ev.Event
  :<|> "event"    :> "transformationEvent" :> ReqBody '[JSON] TransformationEvent :> Post '[JSON] Ev.Event
  :<|> "key"      :> "add"                 :> ReqBody '[JSON] PEM_RSAPubKey       :> Post '[JSON] KeyID
  :<|> "event"    :> "addUser" :> Capture "userID" M.UserID :> Capture "eventID" EventID :> Post '[JSON] ()

type PublicAPI =
       "newUser"  :> ReqBody '[JSON] NewUser            :> Post '[JSON] UserID
  :<|> "key"      :> "get"     :> Capture "keyID" KeyID :> Get '[JSON] PEM_RSAPubKey
  :<|> "key"      :> "getInfo" :> Capture "keyID" KeyID :> Get '[JSON] KeyInfo
  :<|> "business" :> "list"    :> Get '[JSON] [Business]

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

api :: Proxy API'
api = Proxy

type ProtectedAPI = Flat (BasicAuth "foo-realm" User :> PrivateAPI)

type ServerAPI
    =  ProtectedAPI
    :<|> PublicAPI -- :<|> SwaggerAPI


type API
    -- this serves both: swagger.json and swagger-ui
    = SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI

-- To test nested case
type API' = API
    :<|> "nested" :> API
    :<|> SwaggerSchemaUI' "foo-ui" ("foo" :> "swagger.json" :> Get '[JSON] Swagger)
