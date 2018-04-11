{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Endpoint definitions go here. Most of the endpoint definitions are
-- light wrappers around functions in BeamQueries
module Service where

import           API
import qualified AppConfig                        as AC
import qualified BeamQueries                      as BQ
import           Control.Lens                     hiding ((.=))
import           Control.Monad.Except             (runExceptT)
import           Control.Monad.Reader             (liftIO, runReaderT)
import qualified Data.ByteString.Base64           as BS64
import qualified Data.ByteString.Char8            as BSCh
import           Data.Char                        (toLower)
import           Data.Either.Combinators
import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import qualified Data.GS1.Event                   as Ev
import           Data.GS1.EventID
import           Data.GS1.Parser.Parser
import qualified Data.HashMap.Strict.InsOrd       as IOrd
import           Data.Maybe                       (fromJust, isNothing)
import           Data.Swagger
import qualified Data.Text                        as T
import           Data.Text.Encoding               (decodeUtf8)
import           Data.UUID.V4
import           Dummies
import           Errors
import           ErrorUtils                       (appErrToHttpErr,
                                                   throwAppError,
                                                   throwParseError)
import           GHC.TypeLits                     (KnownSymbol)
import           Model                            as M
import qualified OpenSSL.EVP.Digest               as EVPDigest
import           OpenSSL.EVP.PKey                 (PublicKey, SomePublicKey,
                                                   toPublicKey)
import           OpenSSL.EVP.Verify               (VerifyStatus (..), verifyBS)
import           OpenSSL.PEM                      (readPublicKey)
import           OpenSSL.RSA                      (RSAPubKey, rsaSize)
import qualified QueryUtils                       as QU
import           Servant
import           Servant.Server.Experimental.Auth ()
import           Servant.Swagger
import qualified StorageBeam                      as SB
import qualified Utils                            as U

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
authCheck :: AC.Env -> BasicAuthCheck User
authCheck env = do
  let check (BasicAuthData useremail pass) = do
        eitherUser <- AC.runAppM env $ BQ.authCheck (decodeUtf8 useremail) pass
        case eitherUser of
          Right (Just user) -> return (Authorized user)
          Left  _           -> return Unauthorized
    in BasicAuthCheck check

appMToHandler :: forall x. AC.Env -> AC.AppM x -> Handler x
appMToHandler env act = do
  res <- liftIO $ runExceptT $ runReaderT (AC.unAppM act) env
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
        :<|> aggregateEvent user
        :<|> transactionEvent user
        :<|> transformationEvent user
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
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: AC.Env -> Servant.Context ((BasicAuthCheck User) ': '[])
basicAuthServerContext env = (authCheck env) :. EmptyContext

minPubKeySize :: U.Byte
minPubKeySize = U.Byte 256 -- 2048 / 8

addPublicKey :: User -> RSAPublicKey -> AC.AppM KeyID
addPublicKey user pemKey@(PEMString pemStr) = do
  somePubKey <- liftIO $ readPublicKey pemStr
  case checkPubKey somePubKey of
    Just k -> BQ.addPublicKey user k
    _      -> throwAppError $ InvalidRSAKey pemKey

checkPubKey :: SomePublicKey -> Maybe RSAPubKey
checkPubKey spKey
  | isNothing mPKey = Nothing
  | (rsaSize pubKey) < (U.unByte minPubKeySize) = Nothing --rsaSize returns size in bytes
  | otherwise = mPKey
  where
    mPKey :: Maybe RSAPubKey
    mPKey = toPublicKey spKey
    pubKey = fromJust mPKey


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
listEvents :: User ->  M.LabelEPCUrn -> AC.AppM [Ev.Event]
listEvents user urn =
  case (urn2LabelEPC . T.pack $ urn) of
    Left e         -> throwParseError e
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
eventList :: User -> UserID -> AC.AppM [Ev.Event]
eventList user uID = return []

makeDigest :: M.Digest -> IO (Maybe EVPDigest.Digest)
makeDigest d = EVPDigest.getDigestByName $ (map toLower) $ show $ d


{-
   The default padding is PKCS1-1.5, which is deprecated
   for new applications. We should be using PSS instead.

   In the OpenSSL wrapper, the verify function in generic,
   and does not allow you to specify a padding type, as this
   is an RSA specific property.

   I propose we modify OpenSSL to add a function called
   verifyBSRSA :: Digest -> BS -> key -> BS -> PaddingType -> IO VerifyStatus

   We'll need to use the foreign function interface to call:
   EVP_PKEY_CTX_set_rsa_padding in libSSL.

   Lets do this after we have everything compiling.
-}

eventSign :: User -> SignedEvent -> AC.AppM SB.PrimaryKeyType
eventSign user (SignedEvent eventID keyID (Signature sigStr) digest) = do
  event <- BQ.getEventJSON eventID
  rsaPublicKey <- BQ.getPublicKey keyID
  let eSigBS = BS64.decode $ BSCh.pack $ sigStr
  case eSigBS of Left s -> throwAppError $ InvalidSignature s
                 Right sigBS -> do
                  let (PEMString keyStr) = rsaPublicKey
                  sKey <- liftIO $ readPublicKey keyStr
                  let pubKey::RSAPubKey = fromJust $ toPublicKey $ sKey
                      eventBS = QU.eventTxtToBS event
                  maybeDigest <- liftIO $  makeDigest digest
                  verifyStatus <- liftIO $ verifyBS (fromJust maybeDigest) sigBS pubKey eventBS
                  if verifyStatus == VerifySuccess
                     then BQ.insertSignature eventID keyID (Signature sigStr) digest
                     else throwAppError $ InvalidSignature sigStr



-- eventSign user signedEvent = error "Storage module not implemented"
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
objectEvent :: User -> ObjectEvent -> AC.AppM Ev.Event -- SB.PrimaryKeyType
objectEvent = BQ.insertObjectEvent

aggregateEvent :: User -> AggregationEvent -> AC.AppM Ev.Event
aggregateEvent = BQ.insertAggEvent

transactionEvent :: User -> TransactionEvent -> AC.AppM Ev.Event
transactionEvent = BQ.insertTransactEvent

transformationEvent :: User -> TransformationEvent -> AC.AppM Ev.Event
transformationEvent = BQ.insertTransfEvent

sampleEvent:: IO Ev.Event
sampleEvent=  do
  uuid <- nextRandom
  return (Ev.Event Ev.AggregationEventT (Just $ EventID uuid) sampleWhat sampleWhen sampleWhy sampleWhere)


sampleWhat :: DWhat
sampleWhat = dummyObjectDWhat

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

eventInfo :: User -> EventID -> AC.AppM Ev.Event
eventInfo user eID = liftIO sampleEvent

--eventHash :: EventID -> AC.AppM SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
