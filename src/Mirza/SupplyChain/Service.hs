-- {-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
-- {-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Endpoint definitions go here. Most of the endpoint definitions are
-- light wrappers around functions in BeamQueries
module Mirza.SupplyChain.Service where

import           Mirza.SupplyChain.API
import qualified Mirza.SupplyChain.BeamQueries                as BQ
-- import           Mirza.SupplyChain.Dummies     (dummyObjectDWhat)
import           Mirza.SupplyChain.ErrorUtils                 (appErrToHttpErr,
                                                               throwAppError,
                                                               throwParseError)
import           Mirza.SupplyChain.Handlers.Common

import           Mirza.SupplyChain.Handlers.Business
import           Mirza.SupplyChain.Handlers.Contacts
import           Mirza.SupplyChain.Handlers.EventRegistration
import           Mirza.SupplyChain.Handlers.Signatures        hiding
                                                               (getPublicKey)
import qualified Mirza.SupplyChain.QueryUtils                 as QU
import qualified Mirza.SupplyChain.StorageBeam                as SB
import           Mirza.SupplyChain.Types                      hiding
                                                               (NewUser (..),
                                                               User (userId))
import qualified Mirza.SupplyChain.Types                      as ST
import qualified Mirza.SupplyChain.Utils                      as U

import           Data.GS1.DWhat                               (urn2LabelEPC)
import qualified Data.GS1.Event                               as Ev
import           Data.GS1.EventId
import qualified Data.HashMap.Strict.InsOrd                   as IOrd

import           Control.Lens                                 hiding ((.=))
import           Control.Monad.Error.Hoist                    ((<!?>), (<%?>))
import           Control.Monad.IO.Class                       (liftIO)
import qualified Data.ByteString.Base64                       as BS64
import qualified Data.ByteString.Char8                        as BSC
import           Data.Char                                    (toLower)
import           Data.Swagger
import           Data.Text                                    (pack)
import           Data.Text.Encoding                           (decodeUtf8)
import           GHC.TypeLits                                 (KnownSymbol)
import qualified OpenSSL.EVP.Digest                           as EVPDigest
import           OpenSSL.EVP.PKey                             (SomePublicKey,
                                                               toPublicKey)
import           OpenSSL.EVP.Verify                           (VerifyStatus (..),
                                                               verifyBS)
import           OpenSSL.PEM                                  (readPublicKey)
import           OpenSSL.RSA                                  (RSAPubKey,
                                                               rsaSize)
import           Servant
import           Servant.Swagger



appHandlers :: (SCSApp context err, HasScryptParams context) => ServerT ServerAPI (AppM context err)
appHandlers = publicServer :<|> privateServer

publicServer :: (SCSApp context err, HasScryptParams context) => ServerT PublicAPI (AppM context err)
publicServer =
  -- Auth
       newUser
  -- Business
  :<|> getPublicKey
  :<|> getPublicKeyInfo
  :<|> listBusinesses

privateServer :: (SCSApp context err) => ServerT ProtectedAPI (AppM context err)
privateServer =
-- Contacts
       listContacts
  :<|> addContact
  :<|> removeContact
--  :<|> contactsSearch
  :<|> userSearch
-- Signatures
  :<|> addUserToEvent
  :<|> eventSign
  :<|> eventHashed
-- Queries
  :<|> epcState
  :<|> listEvents
  :<|> eventInfo
  :<|> eventList
  :<|> eventUserList
-- Event Registration
  :<|> insertObjectEvent
  :<|> insertAggEvent
  :<|> insertTransactEvent
  :<|> insertTransfEvent
-- Business
  :<|> addPublicKey


instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ =
    let
      authSchemes = IOrd.singleton "basic" $ SecurityScheme SecuritySchemeBasic Nothing
      securityRequirements = [SecurityRequirement $ IOrd.singleton "basic" []]
    in
      toSwagger (Proxy :: Proxy sub)
      & securityDefinitions .~ authSchemes
      & allOperations . security .~ securityRequirements

-- 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: SCSContext -> BasicAuthCheck ST.User
authCheck context =
  let check (BasicAuthData useremail pass) = do
        eitherUser <- runAppM @_ @ServiceError context . runDb $
                      BQ.authCheck (EmailAddress $ decodeUtf8 useremail) (Password pass)
        case eitherUser of
          Right (Just user) -> return (Authorized user)
          _                 -> return Unauthorized
  in BasicAuthCheck check

appMToHandler :: forall x context. context -> AppM context AppError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ runAppM context act
  case res of
    Left (AppError e) -> appErrToHttpErr e
    Right a           -> return a

-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Supplychain Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: SCSContext -> Servant.Context '[BasicAuthCheck ST.User]
basicAuthServerContext context = authCheck context :. EmptyContext





newUser ::  (SCSApp context err, HasScryptParams context)=> ST.NewUser -> AppM context err UserID
newUser = runDb . BQ.newUser



-- PSUEDO:
-- In BeamQueries, implement a function getLabelIDState :: LabelEPCUrn -> IO (_labelID, State)
-- use readLabelEPC in EPC.hs to do it.
-- SELECT * FROM Labels WHERE _labelGs1CompanyPrefix=gs1CompanyPrefix AND _labelType=type AND ...

-- PSUEDO:
-- Use getLabelIDState
epcState :: ST.User ->  LabelEPCUrn -> AppM context err EPCState
epcState _user _str = U.notImplemented

-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
-- PSEUDO:
-- (labelID, _) <- getLabelIDState
-- wholeEvents <- select * from events, dwhats, dwhy, dwhen where _whatItemID=labelID AND _eventID=_whatEventID AND _eventID=_whenEventID AND _eventID=_whyEventID ORDER BY _eventTime;
-- return map constructEvent wholeEvents
listEvents ::  SCSApp context err => ST.User ->  LabelEPCUrn -> AppM context err [Ev.Event]
listEvents _user = either throwParseError (runDb . BQ.listEvents) . urn2LabelEPC . unLabelEPCUrn

-- given an event ID, list all the users associated with that event
-- this can be used to make sure everything is signed
-- PSEUDO:
-- SELECT event.userID, userID1, userID2 FROM Events, BizTransactions WHERE Events._eventID=eventID AND BizTransactionsEventId=Events._eventID;
-- implement a function constructEvent :: WholeEvent -> Event

-- Look into usereventsT and tie that back to the user
-- the function getUser/selectUser might be helpful
eventUserList :: SCSApp context err => ST.User -> EventId -> AppM context err [(ST.User, Bool)]
eventUserList _user = runDb . BQ.eventUserSignedList



userSearch :: ST.User -> String -> AppM context err [ST.User]
-- userSearch user term = liftIO $ Storage.userSearch user term
userSearch _user _term = error "Storage module not implemented"


-- |List events that a particular user was/is involved with
-- use BizTransactions and events (createdby) tables
eventList :: SCSApp context err => ST.User -> UserID -> AppM context err [Ev.Event]
eventList _user = runDb . BQ.eventsByUser



-- | A function to tie a user to an event
-- Populates the ``UserEvents`` table
addUserToEvent :: SCSApp context err => ST.User -> UserID -> EventId -> AppM context err ()
addUserToEvent (User loggedInUserId _ _) anotherUserId eventId =
    runDb $ BQ.addUserToEvent (EventOwner loggedInUserId) (SigningUser anotherUserId) eventId





eventInfo :: SCSApp context err => ST.User -> EventId -> AppM context err (Maybe Ev.Event)
eventInfo _user = runDb . QU.findEvent . SB.EventId . unEventId

--eventHash :: EventId -> AppM context err SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
