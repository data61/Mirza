{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Endpoint definitions go here. Most of the endpoint definitions are
-- light wrappers around functions in BeamQueries
module Service where


import           GHC.TypeLits (KnownSymbol)

import           Servant
import           Servant.Server.Experimental.Auth()
import           Servant.Swagger
import           Data.Swagger
import           Data.GS1.Event
import           Data.GS1.EventID
import           Data.GS1.EPC
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhat
import           Data.GS1.DWhy
import           Data.GS1.Parser.Parser
import           Data.Either.Combinators
import qualified Data.HashMap.Strict.InsOrd as IOrd
import qualified Network.Wai.Handler.Warp as Warp
import           Control.Monad (when)
import           Control.Monad.Reader   (runReaderT, MonadIO,
                                         asks, ask, liftIO)
import           Control.Lens       hiding ((.=))
import           Control.Monad.Except (runExceptT)
import qualified Control.Exception.Lifted as ExL
import           Control.Monad.Trans.Except
import           Data.UUID.V4
import           Data.Text (pack)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT,
                                         asks, ask, liftIO)

-- import qualified Model as M
import qualified AppConfig as AC
import qualified BeamQueries as BQ
import           Utils (debugLog, debugLogGeneral)
import           ErrorUtils (appErrToHttpErr, throwParseError)
import           Model as M
import           API
import qualified StorageBeam as SB

instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ =
    let
      authSchemes = IOrd.singleton "basic" $ SecurityScheme SecuritySchemeBasic Nothing
      securityRequirements = [SecurityRequirement $ IOrd.singleton "basic" []]
    in
      toSwagger (Proxy :: Proxy sub)
      & securityDefinitions .~ authSchemes
      & allOperations . security .~ securityRequirements

-- sampleUser :: User
-- -- sampleUser =  User (Auto Nothing) "Sara" "Falamaki"
-- sampleUser =  User 1 "Sara" "Falamaki"

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
appMToHandler env act = do
  res <- liftIO $ runExceptT $ runReaderT (AC.unAppM act) env
  let envT = AC.envType env
  case res of
    Left (AC.AppError e) -> appErrToHttpErr e
    Right a              -> return a

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
        :<|> objectEvent user
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


addPublicKey :: User -> RSAPublicKey -> AC.AppM KeyID
addPublicKey = BQ.addPublicKey

newUser :: NewUser -> AC.AppM UserID
newUser = BQ.newUser

getPublicKey :: KeyID -> AC.AppM RSAPublicKey
getPublicKey = BQ.getPublicKey

getPublicKeyInfo :: KeyID -> AC.AppM KeyInfo
getPublicKeyInfo = BQ.getPublicKeyInfo

-- PSUEDO:
-- In BeamQueries, implement a function getLabelIDState :: LabelEPCUrn -> IO (_labelID, State)
-- use readLabelEPC in EPC.hs to do it.
-- SELECT * FROM Labels WHERE _labelGs1CompanyPrefix=gs1CompanyPrefix AND _labelType=type AND ...

-- PSUEDO:
-- Use getLabelIDState
epcState :: User ->  M.LabelEPCUrn -> AC.AppM EPCState
epcState user str = return New

-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
-- PSEUDO:
-- (labelID, _) <- getLabelIDState
-- wholeEvents <- select * from events, dwhats, dwhy, dwhen where _whatItemID=labelID AND _eventID=_whatEventID AND _eventID=_whenEventID AND _eventID=_whyEventID ORDER BY _eventTime;
-- return map constructEvent wholeEvents
listEvents :: User ->  M.LabelEPCUrn -> AC.AppM [Event]
listEvents user urn =
  case (urn2LabelEPC . pack $ urn) of
    Left e -> throwParseError e
    Right labelEpc -> BQ.listEvents labelEpc


-- given an event ID, list all the users associated with that event
-- this can be used to make sure everything is signed
-- PSEUDO:
-- SELECT event.userID, userID1, userID2 FROM Events, BizTransactions WHERE Events._eventID=eventID AND BizTransactionsEventId=Events._eventID;
-- implement a function constructEvent :: WholeEvent -> Event
--
eventUserList :: User -> EventID -> AC.AppM [(User, Bool)]
-- eventUserList user eventID = liftIO $ Storage.eventUserList user eventID
eventUserList user eventID = error "Storage module not implemented"


contactsInfo :: User -> AC.AppM [User]
-- contactsInfo user = liftIO $ Storage.listContacts user
contactsInfo user = error "Storage module not implemented"


contactsAdd :: User -> UserID -> AC.AppM Bool
contactsAdd = BQ.addContact

contactsRemove :: User -> UserID -> AC.AppM Bool
contactsRemove = BQ.removeContact

-- Given a search term, search the users contacts for a user matching
-- that term
-- might want to use reg-ex features of postgres10 here:
-- PSEUDO:
-- SELECT user2, firstName, lastName FROM Contacts, Users WHERE user1 LIKE *term* AND user2=Users.id UNION SELECT user1, firstName, lastName FROM Contacts, Users WHERE user2 = ? AND user1=Users.id;" (uid, uid)
contactsSearch :: User -> String -> AC.AppM [User]
contactsSearch user term = return []


userSearch :: User -> String -> AC.AppM [User]
-- userSearch user term = liftIO $ Storage.userSearch user term
userSearch user term = error "Storage module not implemented"

-- select * from Business;
listBusinesses :: AC.AppM [Business]
listBusinesses = error "Implement me"

-- |List events that a particular user was/is involved with
-- use BizTransactions and events (createdby) tables
eventList :: User -> UserID -> AC.AppM [Event]
eventList user uID = return []

eventSign :: User -> SignedEvent -> AC.AppM Bool
eventSign user signedEvent = error "Storage module not implemented"
-- eventSign user signedEvent = do
--   result <- liftIO $ runExceptT $ Storage.eventSign user signedEvent
--   case result of
--     Left SE_NeedMoreSignatures -> return False
--     Left e -> throwError err400 { errBody = LBSC8.pack $ show e }
--     Right () -> return True

-- do we need this?
--
eventHashed :: User -> EventID -> AC.AppM HashedEvent
eventHashed user eventID = return (HashedEvent eventID (EventHash "Blob"))
  {-
eventHashed user eventID = do
  mHash <- liftIO $ Storage.eventHashed user eventID
  case mHash of
    Nothing -> throwError err404 { errBody = "Unknown eventID" }
    Just i -> return i
    -}

-- Return the json encoded copy of the event
objectEvent :: User -> ObjectEvent -> AC.AppM SB.PrimaryKeyType
objectEvent = BQ.insertObjectEvent

eventAggregateObjects :: User -> AggregationEvent -> AC.AppM Event
eventAggregateObjects user aggObject = liftIO sampleEvent

eventDisaggregateObjects :: User -> DisaggregationEvent -> AC.AppM Event
eventDisaggregateObjects user aggObject = liftIO sampleEvent

eventStartTransaction :: User -> TransactionEvent -> AC.AppM Event
eventStartTransaction user aggObject = liftIO sampleEvent

eventTransformObject :: User -> TransformationEvent -> AC.AppM Event
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

eventInfo :: User -> EventID -> AC.AppM Event
eventInfo user eID = liftIO sampleEvent

--eventHash :: EventID -> AC.AppM SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
