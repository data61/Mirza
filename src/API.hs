{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds, KindSignatures #-}

module API where

import Prelude        ()
import Prelude.Compat


import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)



import Servant
import Servant.Server.Experimental.Auth()
import Servant.Swagger
import Servant.Swagger.UI

import GHC.TypeLits (KnownSymbol)

import Data.Aeson
import Data.Aeson.TH
import Data.Swagger
import Data.Maybe
import Data.GS1.Event
import Data.GS1.EventID
import Data.GS1.Object
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy
import Data.Either.Combinators
import Data.Time
import Data.String.Conversions

import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict.InsOrd as IOrd
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai

import Control.Lens       hiding ((.=))

import GHC.Generics       (Generic)

import System.Environment (getArgs, lookupEnv)

import Text.Read          (readMaybe)
import Model

type PrivateAPI =  "rfid" :>  Capture "RFID" String :> "info" :> Get '[JSON] RFIDInfo
            :<|> "event" :> Capture "eventID" EventID:> "info" :> Get '[JSON] EventInfo
            :<|> "contacts" :>  Get '[JSON] [User]
            :<|> "contacts" :> "add" :> Capture "userID" Integer :> Get '[JSON] Bool
            :<|> "contacts" :> "remove" :> Capture "userID" Integer :> Get '[JSON] Bool
            :<|> "contacts" :> "search" :> Capture "term" String :> Get '[JSON] [User]
            :<|> "event" :> "list" :> Capture "userID" Integer :> Get '[JSON] [EventInfo]
            :<|> "event" :> "createObject" :> ReqBody '[JSON] NewObject :> Post '[JSON] ObjectID
            :<|> "event" :> "aggregateObjects" :> ReqBody '[JSON] AggregatedObject :> Post '[JSON] EventInfo
            :<|> "event" :> "start-transaction" :> ReqBody '[JSON] TransactionInfo :> Post '[JSON] EventInfo
            :<|> "event" :> "transformObject" :> ReqBody '[JSON] TransformationInfo :> Post '[JSON] EventInfo
            :<|> "key" :> "add" :>  ReqBody '[OctetStream] BinaryBlob :> Post '[JSON] KeyID


type PublicAPI =   "newUser" :> ReqBody '[JSON] NewUser :> Post '[JSON] UserID
            :<|> "key" :> "get" :> Capture "keyID" KeyID :> Get '[OctetStream] BinaryBlob
            :<|> "key" :> "getInfo" :> Capture "keyID" KeyID :> Get '[JSON] KeyInfo

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
