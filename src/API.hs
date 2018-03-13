{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE DataKinds             #-}

module API where

-- import           Prelude        ()
-- import           Prelude.Compat
import           Servant
-- import           Servant.Server.Experimental.Auth()
import           Servant.Swagger.UI
import           Data.Swagger
import           Data.GS1.Event
import           Data.GS1.EventID
import           Model as M
import           StorageBeam (PrimaryKeyType)

type PrivateAPI =
                 "epc" :>  Capture "urn" String:> "info" :> Get '[JSON] EPCState
            :<|> "epc" :> Capture "urn" String :> "events" :> Get '[JSON] [Event]
            :<|> "event" :> Capture "eventID" EventID:> "info" :> Get '[JSON] Event
            :<|> "contacts" :>  Get '[JSON] [User]
            :<|> "contacts" :> "add" :> Capture "userID" PrimaryKeyType :> Get '[JSON] Bool
            :<|> "contacts" :> "remove" :> Capture "userID" PrimaryKeyType :> Get '[JSON] Bool
            :<|> "contacts" :> "search" :> Capture "term" String :> Get '[JSON] [User]
            :<|> "event" :> "list" :> Capture "userID" PrimaryKeyType :> Get '[JSON] [Event]
            :<|> "event" :> "listUsers" :> Capture "eventID" EventID :> Get '[JSON] [(User, Bool)]
            :<|> "event" :> "sign" :> ReqBody '[JSON] SignedEvent :> Post '[JSON] Bool
            :<|> "event" :> "getHash" :> ReqBody '[JSON] EventID :> Post '[JSON] HashedEvent
            :<|> "event" :> "objectEvent" :> ReqBody '[JSON] ObjectEvent :> Post '[JSON] PrimaryKeyType
            :<|> "event" :> "aggregateObjects" :> ReqBody '[JSON] AggregatedObject :> Post '[JSON] Event
            :<|> "event" :> "disAggregateObjects" :> ReqBody '[JSON] DisaggregatedObject :> Post '[JSON] Event
            :<|> "event" :> "start-transaction" :> ReqBody '[JSON] TransactionInfo :> Post '[JSON] Event
            :<|> "event" :> "transformObject" :> ReqBody '[JSON] TransformationInfo :> Post '[JSON] Event
            :<|> "key" :> "add" :>  ReqBody '[PlainText] PEMString :> Post '[JSON] KeyID

type PublicAPI =   "newUser" :> ReqBody '[JSON] NewUser :> Post '[JSON] UserID
            :<|> "key" :> "get" :> Capture "keyID" KeyID :> Get '[JSON] RSAPublicKey
            :<|> "key" :> "getInfo" :> Capture "keyID" KeyID :> Get '[JSON] KeyInfo
            :<|> "business" :> "list" :> Get '[JSON] [Business]

type SwaggerAPI = SwaggerSchemaUI "swagger-ui" "swagger.json"

api :: Proxy API'
api = Proxy

type ServerAPI =  BasicAuth "foo-realm" User :> PrivateAPI :<|> PublicAPI -- :<|> SwaggerAPI


type API =
    -- this serves both: swagger.json and swagger-ui
    SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> ServerAPI

-- To test nested case
type API' = API
    :<|> "nested" :> API
    :<|> SwaggerSchemaUI' "foo-ui" ("foo" :> "swagger.json" :> Get '[JSON] Swagger)
