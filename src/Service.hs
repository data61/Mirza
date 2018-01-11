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

-- import Database.SQLite.Simple as Sql hiding ((:.))
import Database.Beam as B
import Database.Beam.Postgres
import Database.PostgreSQL.Simple hiding ((:.))
import Database.Beam.Backend
import Database.Beam.Backend.SQL.BeamExtensions
import Database.PostgreSQL.Simple.FromField
import Database.Beam.Backend.SQL
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
type DBConn = Connection

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
authCheck :: DBConn -> BasicAuthCheck User
authCheck conn = error "Storage module not implemented"
  -- let check (BasicAuthData username password) = do
  --       maybeUser <- Storage.authCheck conn username password
  --       case maybeUser of
  --          Nothing -> return Unauthorized
  --          (Just user) -> return (Authorized user)
  -- in BasicAuthCheck check



privateServer :: DBConn -> User -> Server PrivateAPI
privateServer conn user =
             epcState conn user
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

publicServer :: DBConn -> Server PublicAPI
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
basicAuthServerContext :: DBConn -> Servant.Context (BasicAuthCheck User ': '[])
basicAuthServerContext conn = authCheck conn :. EmptyContext


addPublicKey :: DBConn -> User -> RSAPublicKey -> Handler KeyID
addPublicKey conn user sig = error "Storage module not implemented"
  -- liftIO (Storage.addPublicKey conn user sig)


newUser :: DBConn -> NewUser -> Handler UserID
newUser conn nu = error "Storage module not implemented"
  -- liftIO (Storage.newUser conn nu)


getPublicKey :: DBConn -> KeyID -> Handler RSAPublicKey
getPublicKey conn keyID = error "Storage module not implemented"
  -- do
  --   result <- liftIO $ runExceptT $ Storage.getPublicKey conn keyID
  --   case result of
  --     Left e -> throwError err400 { errBody = LBSC8.pack $ show e}
  --     Right key -> return key


getPublicKeyInfo :: DBConn -> KeyID -> Handler KeyInfo
getPublicKeyInfo conn keyID = error "Storage module not implemented"
-- getPublicKeyInfo conn keyID = do
--   result <- liftIO $ runExceptT $ Storage.getPublicKeyInfo conn keyID
--   case result of
--     Left e -> throwError err404 { errBody = LBSC8.pack $ show e }
--     Right keyInfo -> return keyInfo

-- PSUEDO:
-- In BeamQueries, implement a function getLabelIDState :: EPCUrn -> IO (_labelID, State)
-- use readLabelEPC in EPC.hs to do it.
-- SELECT * FROM Labels WHERE _labelGs1CompanyPrefix=gs1CompanyPrefix AND _labelType=type AND ...

--
--
--

-- PSUEDO:
-- Use getLabelIDState
epcState :: DBConn -> User ->  EPCUrn -> Handler EPCState
epcState conn user str = return New

-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
-- PSEUDO:
-- (labelID, _) <- getLabelIDState
-- eventIDs <- SELECT eventID FROM DWhat WHERE _labelID=labelID;
-- wholeEvents <- getWholeEvents
-- return constructEvents wholeEvents

-- wholeEvents <- select * from events, dwhats, dwhy, dwhen where _whatItemID=2 AND _eventID=_whatEventID AND _eventID=_whenEventID AND _eventID=_whyEventID;
-- return map constructEvent wholeEvents
listEvents :: DBConn -> User ->  EPCUrn -> Handler [Event]
listEvents conn user urn = return []


-- given an event ID, list all the users associated with that event
-- this can be used to make sure everything is signed
-- PSEUDO:
-- SELECT event.userID, userID1, userID2 FROM Events, BizTransactions WHERE Events._eventID=eventID AND BizTransactionsId=Events._eventID;
-- implement a function constructEvent :: WholeEvent -> Event
--
eventUserList :: DBConn -> User -> EventID -> Handler [(User, Bool)]
-- eventUserList conn user eventID = liftIO $ Storage.eventUserList conn user eventID
eventUserList conn user eventID = error "Storage module not implemented"


contactsInfo :: DBConn -> User -> Handler [User]
-- contactsInfo conn user = liftIO $ Storage.listContacts conn user
contactsInfo conn user = error "Storage module not implemented"


contactsAdd :: DBConn -> User -> UserID -> Handler Bool
-- contactsAdd conn user userId = liftIO (Storage.addContacts conn user userId)
contactsAdd conn user userId = error "Storage module not implemented"


contactsRemove :: DBConn -> User -> UserID -> Handler Bool
-- contactsRemove conn user userId = liftIO (Storage.removeContacts conn user userId)
contactsRemove conn user userId = error "Storage module not implemented"


-- Given a search term, search the users contacts for a user matching
-- that term
-- might want to use reg-ex features of postgres10 here:
-- PSEUDO:
-- SELECT user2, firstName, lastName FROM Contacts, Users WHERE user1 LIKE *term* AND user2=Users.id UNION SELECT user1, firstName, lastName FROM Contacts, Users WHERE user2 = ? AND user1=Users.id;" (uid, uid)
contactsSearch :: DBConn -> User -> String -> Handler [User]
contactsSearch conn user term = return []


userSearch :: DBConn -> User -> String -> Handler [User]
-- userSearch conn user term = liftIO $ Storage.userSearch conn user term
userSearch conn user term = error "Storage module not implemented"


eventList :: DBConn -> User -> UserID -> Handler [Event]
eventList conn user uID = return []

eventSign :: DBConn -> User -> SignedEvent -> Handler Bool
eventSign conn user signedEvent = error "Storage module not implemented"
-- eventSign conn user signedEvent = do
--   result <- liftIO $ runExceptT $ Storage.eventSign conn user signedEvent
--   case result of
--     Left SE_NeedMoreSignatures -> return False
--     Left e -> throwError err400 { errBody = LBSC8.pack $ show e }
--     Right () -> return True

-- do we need this?
--
eventHashed :: DBConn -> User -> EventID -> Handler HashedEvent
eventHashed conn user eventID = return (HashedEvent eventID (EventHash "Blob"))
  {-
eventHashed conn user eventID = do
  mHash <- liftIO $ Storage.eventHashed conn user eventID
  case mHash of
    Nothing -> throwError err404 { errBody = "Unknown eventID" }
    Just i -> return i
    -}

-- Return the json encoded copy of the event
eventCreateObject :: DBConn -> User -> NewObject -> Handler Event
eventCreateObject conn user newObject = error "Storage module not implemented"
  -- liftIO (Storage.eventCreateObject conn user newObject)

eventAggregateObjects :: DBConn -> User -> AggregatedObject -> Handler Event
eventAggregateObjects conn user aggObject = liftIO sampleEvent

eventDisaggregateObjects :: DBConn -> User -> DisaggregatedObject -> Handler Event
eventDisaggregateObjects conn user aggObject = liftIO sampleEvent

eventStartTransaction :: DBConn -> User -> TransactionInfo -> Handler Event
eventStartTransaction conn user aggObject = liftIO sampleEvent

eventTransformObject :: DBConn -> User -> TransformationInfo -> Handler Event
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

eventInfo :: DBConn -> User -> EventID -> Handler Event
eventInfo conn user eID = liftIO sampleEvent

--eventHash :: EventID -> Handler SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
