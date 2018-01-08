{-# LANGUAGE DataKinds             #-}
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
module Service where

import Model
import API
import qualified Storage

import Prelude        ()
import Prelude.Compat

import Database.SQLite.Simple as Sql hiding ((:.))

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)


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
import Data.GS1.EventID
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy
import Data.GS1.Parser.Parser
import Data.Either.Combinators
import Data.Time
import Data.String.Conversions


import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict.InsOrd as IOrd
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai

import Crypto.PubKey.RSA

import Control.Lens       hiding ((.=))

import GHC.Generics       (Generic)

import System.Environment (getArgs, lookupEnv)

import Text.Read          (readMaybe)
import qualified Data.Text as Txt

import Control.Monad.Except
-- remove me eventually
import Data.UUID.V4

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
           (Just user) -> return (Authorized user)
  in BasicAuthCheck check



privateServer :: Sql.Connection -> User -> Server PrivateAPI
privateServer conn user =
             epcInfo conn user
        :<|> listEvents conn user
        :<|> eventInfo conn user
        :<|> contactsInfo conn user
        :<|> contactsAdd conn user
        :<|> contactsRemove conn user
--        :<|> contactsSearch conn user
        :<|> userSearch conn user
        :<|> eventList conn user
        :<|> eventUserList conn user
        :<|> eventSign conn user
        :<|> eventHashed conn user
        :<|> eventCreateObject conn user
        :<|> eventAggregateObjects conn user
        :<|> eventDisaggregateObjects conn user
        :<|> eventStartTransaction conn user
        :<|> eventTransformObject conn user
        :<|> Service.addPublicKey conn user

          {-
        :<|> return . eventHash
        -}

publicServer :: Sql.Connection -> Server PublicAPI
publicServer conn =  Service.newUser conn
    :<|>  Service.getPublicKey conn
    :<|>  Service.getPublicKeyInfo conn



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
basicAuthServerContext conn = authCheck conn :. EmptyContext


addPublicKey :: Sql.Connection -> User -> RSAPublicKey -> Handler KeyID
addPublicKey conn user sig = liftIO (Storage.addPublicKey conn user sig)


newUser :: Sql.Connection -> NewUser -> Handler UserID
newUser conn nu = liftIO (Storage.newUser conn nu)


getPublicKey :: Sql.Connection -> KeyID -> Handler RSAPublicKey
getPublicKey conn keyID = do
  result <- liftIO $ runExceptT $ Storage.getPublicKey conn keyID
  case result of
    Left e -> throwError err400 { errBody = LBSC8.pack $ show e}
    Right key -> return key


getPublicKeyInfo :: Sql.Connection -> KeyID -> Handler KeyInfo
getPublicKeyInfo conn keyID = do
  result <- liftIO $ runExceptT $ Storage.getPublicKeyInfo conn keyID
  case result of
    Left e -> throwError err404 { errBody = LBSC8.pack $ show e }
    Right keyInfo -> return keyInfo

epcInfo :: Sql.Connection -> User ->  String -> Handler EPCInfo
epcInfo conn user str = return (EPCInfo New Nothing)

-- This takes an EPC url, find the object's ID (from DB?) or maybe we just hash it?
-- and then looks up all the events related to that item.
listEvents :: Sql.Connection -> User ->  String -> Handler [Event]
listEvents conn user str = return []


-- given an event ID, list all the users associated with that event
-- this can be used to make sure everything is signed
eventUserList :: Sql.Connection -> User -> EventID -> Handler [(User, Bool)]
eventUserList conn user eventID = liftIO $ Storage.eventUserList conn user eventID


contactsInfo :: Sql.Connection -> User -> Handler [User]
contactsInfo conn user = liftIO $ Storage.listContacts conn user


contactsAdd :: Sql.Connection -> User -> UserID -> Handler Bool
contactsAdd conn user userId = liftIO (Storage.addContacts conn user userId)


contactsRemove :: Sql.Connection -> User -> UserID -> Handler Bool
contactsRemove conn user userId = liftIO (Storage.removeContacts conn user userId)


contactsSearch :: Sql.Connection -> User -> String -> Handler [User]
contactsSearch conn user term = return []


userSearch :: Sql.Connection -> User -> String -> Handler [User]
userSearch conn user term = liftIO $ Storage.userSearch conn user term


eventList :: Sql.Connection -> User -> UserID -> Handler [Event]
eventList conn user uID = return []

eventSign :: Sql.Connection -> User -> SignedEvent -> Handler Bool
eventSign conn user signedEvent = do
  result <- liftIO $ runExceptT $ Storage.eventSign conn user signedEvent
  case result of
    Left SE_NeedMoreSignatures -> return False
    Left e -> throwError err400 { errBody = LBSC8.pack $ show e }
    Right () -> return True

-- do we need this?
--
eventHashed :: Sql.Connection -> User -> EventID -> Handler HashedEvent
eventHashed conn user eventID = return (HashedEvent eventID (EventHash "Blob"))
  {-
eventHashed conn user eventID = do
  mHash <- liftIO $ Storage.eventHashed conn user eventID
  case mHash of
    Nothing -> throwError err404 { errBody = "Unknown eventID" }
    Just i -> return i
    -}

-- Return the json encoded copy of the event
eventCreateObject :: Sql.Connection -> User -> NewObject -> Handler Event
eventCreateObject conn user newObject =
  liftIO (Storage.eventCreateObject conn user newObject)

eventAggregateObjects :: Sql.Connection -> User -> AggregatedObject -> Handler Event
eventAggregateObjects conn user aggObject = liftIO sampleEvent

eventDisaggregateObjects :: Sql.Connection -> User -> DisaggregatedObject -> Handler Event
eventDisaggregateObjects conn user aggObject = liftIO sampleEvent

eventStartTransaction :: Sql.Connection -> User -> TransactionInfo -> Handler Event
eventStartTransaction conn user aggObject = liftIO sampleEvent

eventTransformObject :: Sql.Connection -> User -> TransformationInfo -> Handler Event
eventTransformObject conn user aggObject = liftIO sampleEvent

sampleEvent:: IO Event
sampleEvent=  do
  uuid <- nextRandom
  return (Event AggregationEventT (Just $ EventID uuid) sampleWhat sampleWhen sampleWhy sampleWhere)


sampleWhat :: DWhat
sampleWhat = ObjectDWhat Observe [IL (GIAI "2020939" "029393")]

sampleWhy :: DWhy
sampleWhy = DWhy (Just Arriving) (Just Data.GS1.EPC.Active)

sampleWhen :: DWhen
sampleWhen = DWhen pt (Just pt) tz
  where
      t = "2017-01-24T13:08:24.11+10:00"
      pt = fromRight' (parseStr2Time t)
      tz = fromRight' (parseStr2TimeZone t)

sampleWhere :: DWhere
sampleWhere = DWhere [] [] [] []

eventInfo :: Sql.Connection -> User -> EventID -> Handler Event
eventInfo conn user eID = liftIO sampleEvent

--eventHash :: EventID -> Handler SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
