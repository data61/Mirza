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

import           Model
import           API
-- import qualified StorageBeam

import           Prelude.Compat

-- import Database.SQLite.Simple as Sql hiding ((:.))
import           Database.Beam as B
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple hiding ((:.))
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.PostgreSQL.Simple.FromField
import           Database.Beam.Backend.SQL
import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)
import qualified Control.Category as C

import           GHC.TypeLits (KnownSymbol)

import           Servant
import           Servant.Server (enter)
import           Servant.Server.Experimental.Auth()
import           Servant.Swagger
import           Servant.Swagger.UI

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import           Data.Maybe
import           Data.GS1.Event
import           Data.GS1.EventID
import           Data.GS1.EPC
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhat
import           Data.GS1.DWhy
import           Data.GS1.Parser.Parser
import           Data.Either.Combinators
import           Data.Time
import           Data.String.Conversions


import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import qualified Data.HashMap.Strict.InsOrd as IOrd
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT,
                                         asks, ask, liftIO)
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
import qualified BeamQueries as BQ
import qualified AppConfig as AC

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
-- sampleUser =  User (Auto Nothing) "Sara" "Falamaki"
sampleUser =  User 1 "Sara" "Falamaki"

-- 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck User
authCheck = error "Storage module not implemented"
  -- let check (BasicAuthData username password) = do
  --       maybeUser <- Storage.authCheck username password
  --       case maybeUser of
  --          Nothing -> return Unauthorized
  --          (Just user) -> return (Authorized user)
  -- in BasicAuthCheck check


appMToHandler :: forall x. AC.Env -> AC.AppM x -> Handler x
appMToHandler env = liftIO . flip runReaderT env . AC.unAppM

privateServer :: User -> ServerT PrivateAPI AC.AppM
privateServer user =
             epcState user
        :<|> listEvents user
        :<|> eventInfo user
        :<|> contactsInfo user
        :<|> contactsAdd user
        :<|> contactsRemove user
--        :<|> contactsSearch user
        :<|> userSearch user
        :<|> eventList user
        :<|> eventUserList user
        :<|> eventSign user
        :<|> eventHashed user
        :<|> eventCreateObject user
        :<|> eventAggregateObjects user
        :<|> eventDisaggregateObjects user
        :<|> eventStartTransaction user
        :<|> eventTransformObject user
        :<|> Service.addPublicKey user

publicServer :: ServerT PublicAPI AC.AppM
publicServer =     Service.newUser
              :<|> Service.getPublicKey
              :<|> Service.getPublicKeyInfo
              :<|> Service.listBusinesses
              
appHandlers = privateServer :<|> publicServer

serverAPI :: Proxy ServerAPI
serverAPI = Proxy

-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Supplychain Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")


-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Servant.Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext


addPublicKey :: User -> RSAPublicKey -> Handler KeyID
addPublicKey user sig = error "Storage module not implemented"
  -- liftIO (Storage.addPublicKey user sig)


newUser :: NewUser -> Handler UserID
-- newUser nu = error "Storage module not implemented"
--   -- liftIO (Storage.newUser nu)
-- newUser nu = BQ.newUser nu
newUser nu = error "not implemented yet"

  -- pure uId

getPublicKey :: KeyID -> Handler RSAPublicKey
getPublicKey keyID = error "Storage module not implemented"
  -- do
  --   result <- liftIO $ runExceptT $ Storage.getPublicKey keyID
  --   case result of
  --     Left e -> throwError err400 { errBody = LBSC8.pack $ show e}
  --     Right key -> return key


getPublicKeyInfo :: KeyID -> Handler KeyInfo
getPublicKeyInfo keyID = error "Storage module not implemented"
-- getPublicKeyInfo keyID = do
--   result <- liftIO $ runExceptT $ Storage.getPublicKeyInfo keyID
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
epcState :: User ->  EPCUrn -> Handler EPCState
epcState user str = return New

-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
-- PSEUDO:
-- (labelID, _) <- getLabelIDState
-- wholeEvents <- select * from events, dwhats, dwhy, dwhen where _whatItemID=labelID AND _eventID=_whatEventID AND _eventID=_whenEventID AND _eventID=_whyEventID ORDER BY _eventTime;
-- return map constructEvent wholeEvents
listEvents :: User ->  EPCUrn -> Handler [Event]
listEvents user urn = return []


-- given an event ID, list all the users associated with that event
-- this can be used to make sure everything is signed
-- PSEUDO:
-- SELECT event.userID, userID1, userID2 FROM Events, BizTransactions WHERE Events._eventID=eventID AND BizTransactionsEventId=Events._eventID;
-- implement a function constructEvent :: WholeEvent -> Event
--
eventUserList :: User -> EventID -> Handler [(User, Bool)]
-- eventUserList user eventID = liftIO $ Storage.eventUserList user eventID
eventUserList user eventID = error "Storage module not implemented"


contactsInfo :: User -> Handler [User]
-- contactsInfo user = liftIO $ Storage.listContacts user
contactsInfo user = error "Storage module not implemented"


contactsAdd :: User -> UserID -> Handler Bool
-- contactsAdd user userId = liftIO (Storage.addContacts user userId)
contactsAdd user userId = error "Storage module not implemented"


contactsRemove :: User -> UserID -> Handler Bool
-- contactsRemove user userId = liftIO (Storage.removeContacts user userId)
contactsRemove user userId = error "Storage module not implemented"


-- Given a search term, search the users contacts for a user matching
-- that term
-- might want to use reg-ex features of postgres10 here:
-- PSEUDO:
-- SELECT user2, firstName, lastName FROM Contacts, Users WHERE user1 LIKE *term* AND user2=Users.id UNION SELECT user1, firstName, lastName FROM Contacts, Users WHERE user2 = ? AND user1=Users.id;" (uid, uid)
contactsSearch :: User -> String -> Handler [User]
contactsSearch user term = return []


userSearch :: User -> String -> Handler [User]
-- userSearch user term = liftIO $ Storage.userSearch user term
userSearch user term = error "Storage module not implemented"

-- select * from Business;
listBusinesses :: Handler [Business]
listBusinesses = error "Implement me"

-- |List events that a particular user was/is involved with
-- use BizTransactions and events (createdby) tables
eventList :: User -> UserID -> Handler [Event]
eventList user uID = return []

eventSign :: User -> SignedEvent -> Handler Bool
eventSign user signedEvent = error "Storage module not implemented"
-- eventSign user signedEvent = do
--   result <- liftIO $ runExceptT $ Storage.eventSign user signedEvent
--   case result of
--     Left SE_NeedMoreSignatures -> return False
--     Left e -> throwError err400 { errBody = LBSC8.pack $ show e }
--     Right () -> return True

-- do we need this?
--
eventHashed :: User -> EventID -> Handler HashedEvent
eventHashed user eventID = return (HashedEvent eventID (EventHash "Blob"))
  {-
eventHashed user eventID = do
  mHash <- liftIO $ Storage.eventHashed user eventID
  case mHash of
    Nothing -> throwError err404 { errBody = "Unknown eventID" }
    Just i -> return i
    -}

-- Return the json encoded copy of the event
eventCreateObject :: User -> NewObject -> Handler Event
eventCreateObject user newObject = error "Storage module not implemented"
  -- liftIO (Storage.eventCreateObject user newObject)

eventAggregateObjects :: User -> AggregatedObject -> Handler Event
eventAggregateObjects user aggObject = liftIO sampleEvent

eventDisaggregateObjects :: User -> DisaggregatedObject -> Handler Event
eventDisaggregateObjects user aggObject = liftIO sampleEvent

eventStartTransaction :: User -> TransactionInfo -> Handler Event
eventStartTransaction user aggObject = liftIO sampleEvent

eventTransformObject :: User -> TransformationInfo -> Handler Event
eventTransformObject user aggObject = liftIO sampleEvent

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

eventInfo :: User -> EventID -> Handler Event
eventInfo user eID = liftIO sampleEvent

--eventHash :: EventID -> Handler SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
