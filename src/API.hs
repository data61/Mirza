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

module API where

-- import           Prelude        ()
-- import           Prelude.Compat
import           Servant
import           Servant.API.Flatten
-- import           Servant.Server.Experimental.Auth()
import qualified Data.GS1.Event      as Ev
import           Data.GS1.EventID
import           Data.Swagger
import           Model               as M
import           Servant.Swagger.UI
import           StorageBeam         (PrimaryKeyType)

type PrivateAPI =
       "epc"      :> Capture "urn" String      :> "info"   :> Get '[JSON] EPCState
  :<|> "epc"      :> Capture "urn" String      :> "events" :> Get '[JSON] [Ev.Event]
  :<|> "event"    :> Capture "eventID" EventID :> "info"   :> Get '[JSON] Ev.Event
  :<|> "contacts" :>  Get '[JSON] [User]
  :<|> "contacts" :> "add"               :> Capture "userID" PrimaryKeyType     :> Get '[JSON] Bool
  :<|> "contacts" :> "remove"            :> Capture "userID" PrimaryKeyType     :> Get '[JSON] Bool
  :<|> "contacts" :> "search"            :> Capture "term" String               :> Get '[JSON] [User]
  :<|> "event"    :> "list"              :> Capture "userID" PrimaryKeyType     :> Get '[JSON] [Ev.Event]
  :<|> "event"    :> "listUsers"         :> Capture "eventID" EventID           :> Get '[JSON] [(User, Bool)]
  :<|> "event"    :> "sign"              :> ReqBody '[JSON] SignedEvent         :> Post '[JSON] PrimaryKeyType
  :<|> "event"    :> "getHash"           :> ReqBody '[JSON] EventID             :> Post '[JSON] HashedEvent
  :<|> "event"    :> "objectEvent"       :> ReqBody '[JSON] ObjectEvent         :> Post '[JSON] Ev.Event
  :<|> "event"    :> "aggregateObjects"  :> ReqBody '[JSON] AggregationEvent    :> Post '[JSON] Ev.Event
  :<|> "event"    :> "start-transaction" :> ReqBody '[JSON] TransactionEvent    :> Post '[JSON] Ev.Event
  :<|> "event"    :> "transformObject"   :> ReqBody '[JSON] TransformationEvent :> Post '[JSON] Ev.Event
  :<|> "key"      :> "add"               :> ReqBody '[JSON] RSAPublicKey        :> Post '[JSON] KeyID

type PublicAPI =
       "newUser"  :> ReqBody '[JSON] NewUser            :> Post '[JSON] UserID
  :<|> "key"      :> "get"     :> Capture "keyID" KeyID :> Get '[JSON] RSAPublicKey
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
