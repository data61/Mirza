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
module Service where

import Model
import API
import qualified Storage

import Prelude        ()
import Prelude.Compat

import Database.SQLite.Simple as Sql hiding ((:.))

import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)


import GHC.TypeLits (KnownSymbol)

import Servant
import Servant.Server.Experimental.Auth()
import Servant.Swagger
import Servant.Swagger.UI

import Data.Aeson
import Data.Aeson.TH
import Data.Swagger
import Data.Maybe
import Data.GS1.Event
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

instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ =
    let
      authSchemes = IOrd.singleton "basic" $ SecurityScheme SecuritySchemeBasic Nothing
      securityRequirements = [SecurityRequirement $ IOrd.singleton "basic" []]
    in
      toSwagger (Proxy :: Proxy sub)
      & securityDefinitions .~ authSchemes
      & allOperations . security .~ securityRequirements



sampleUser :: User
sampleUser =  User 1 "Sara" "Falamaki"

-- 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: Sql.Connection -> BasicAuthCheck User
authCheck conn =
  let check (BasicAuthData username password) = do
        maybeUser <- Storage.authCheck conn username password
        case maybeUser of
           Nothing -> return Unauthorized
           (Just user) -> return (Authorized (user))
  in BasicAuthCheck check


publicServer :: Sql.Connection -> Server PublicAPI
publicServer conn =  Service.newUser conn


privateServer :: Sql.Connection -> User -> Server PrivateAPI
privateServer conn user =  return . rfid
        :<|> return . eventInfo
        :<|> return . contactsInfo
        :<|> return . contactsAdd
        :<|> return . contactsRemove
        :<|> return . contactsSearch
        :<|> return . eventList
        :<|> return . eventCreateObject
        :<|> return . eventAggregateObjects
        :<|> return . eventStartTransaction
        :<|> return . eventTransformObject
        :<|> (Service.addPublicKey conn user)
        :<|> return . getPublicKey

          {-
        :<|> return . eventHash
        -}


-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Supplychain Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")


serverAPI :: Proxy ServerAPI
serverAPI = Proxy

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Sql.Connection -> Servant.Context (BasicAuthCheck User ': '[])
basicAuthServerContext conn = (authCheck conn):. EmptyContext


addPublicKey :: MonadIO m => Sql.Connection -> User -> BinaryBlob -> m KeyID
addPublicKey conn user sig = liftIO (Storage.addPublicKey conn user sig)


getPublicKey :: UserID -> BinaryBlob
getPublicKey userID = BinaryBlob ByteString.empty

newUser :: MonadIO m => Sql.Connection -> NewUser -> m UserID
newUser conn nu = liftIO (Storage.newUser conn nu)


rfid :: String -> Maybe RFIDInfo
rfid str = Just (RFIDInfo New Nothing)



sampleWhat :: DWhat
sampleWhat = ObjectDWhat Observe [GLN "urn:epc:id:sgtin:0614141" "107346" "2017", GLN "urn:epc:id:sgtin:0614141" "107346" "2018"] []

sampleWhy :: DWhy
sampleWhy = DWhy (Just Arriving) (Just Data.GS1.DWhy.Active)

sampleWhen :: DWhen
sampleWhen = DWhen pt (Just pt) tz
  where
      t = "2017-01-24T13:08:24.11+10:00"
      pt = fromRight' (parseStr2Time t :: Either EPCISTimeError EPCISTime)
      tz = fromRight' (parseStr2TimeZone t :: Either EPCISTimeError TimeZone)


eventInfo :: EventID -> EventInfo
eventInfo eID = EventInfo 1 AggregationEventT New sampleWhat sampleWhy sampleWhen []

eventHash :: EventID -> SignedEvent
eventHash eID = SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2]


contactsInfo :: UserID -> [User]
contactsInfo uID = []

contactsAdd :: UserID -> Bool
contactsAdd uID = False

contactsRemove :: UserID -> Bool
contactsRemove uID = False

contactsSearch :: String -> [User]
contactsSearch term = []

eventList :: UserID -> [EventInfo]
eventList uID = [(eventInfo 1)]

eventCreateObject :: NewObject -> ObjectID
eventCreateObject newObject = "newObjectID"

eventAggregateObjects :: AggregatedObject -> EventInfo
eventAggregateObjects _ = eventInfo 1

eventStartTransaction :: TransactionInfo -> EventInfo
eventStartTransaction _ = eventInfo 1

eventTransformObject :: TransformationInfo -> EventInfo
eventTransformObject _ = eventInfo 1

