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
import qualified Mirza.SupplyChain.BeamQueries as BQ
-- import           Mirza.SupplyChain.Dummies     (dummyObjectDWhat)
import           Mirza.SupplyChain.ErrorUtils  (appErrToHttpErr, throwAppError,
                                                throwParseError)
import qualified Mirza.SupplyChain.QueryUtils  as QU
import qualified Mirza.SupplyChain.StorageBeam as SB
import           Mirza.SupplyChain.Types
import qualified Mirza.SupplyChain.Types       as AC
import qualified Mirza.SupplyChain.Utils       as U

import           Data.GS1.DWhat                (urn2LabelEPC)
import qualified Data.GS1.Event                as Ev
import           Data.GS1.EventId
import qualified Data.HashMap.Strict.InsOrd    as IOrd

import           Control.Lens                  hiding ((.=))
import           Control.Monad.Error.Hoist     ((<!?>), (<%?>))
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString.Base64        as BS64
import qualified Data.ByteString.Char8         as BSC
import           Data.Char                     (toLower)
import           Data.Swagger
import           Data.Text                     (pack)
import           Data.Text.Encoding            (decodeUtf8)
import           GHC.TypeLits                  (KnownSymbol)
import qualified OpenSSL.EVP.Digest            as EVPDigest
import           OpenSSL.EVP.PKey              (SomePublicKey, toPublicKey)
import           OpenSSL.EVP.Verify            (VerifyStatus (..), verifyBS)
import           OpenSSL.PEM                   (readPublicKey)
import           OpenSSL.RSA                   (RSAPubKey, rsaSize)
import           Servant
import           Servant.Swagger

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
authCheck :: AC.SCSContext -> BasicAuthCheck User
authCheck context =
  let check (BasicAuthData useremail pass) = do
        eitherUser <- AC.runAppM @_ @ServiceError context . AC.runDb $
                      BQ.authCheck (EmailAddress $ decodeUtf8 useremail) (Password pass)
        case eitherUser of
          Right (Just user) -> return (Authorized user)
          _                 -> return Unauthorized
  in BasicAuthCheck check

appMToHandler :: forall x context. context -> AC.AppM context AC.AppError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ AC.runAppM context act
  case res of
    Left (AC.AppError e) -> appErrToHttpErr e
    Right a              -> return a

privateServer :: (SCSApp context err) => ServerT ProtectedAPI (AC.AppM context err)
privateServer
  =    epcState
  :<|> listEvents
  :<|> eventInfo
  :<|> listContacts
  :<|> addContact
  :<|> removeContact
--        :<|> contactsSearch
  :<|> userSearch
  :<|> eventList
  :<|> eventUserList
  :<|> eventSign
  :<|> eventHashed
  :<|> insertObjectEvent
  :<|> insertAggEvent
  :<|> insertTransactEvent
  :<|> insertTransfEvent
  :<|> addUserToEvent
  :<|> addPublicKey

publicServer :: (SCSApp context err, HasScryptParams context) => ServerT PublicAPI (AC.AppM context err)
publicServer
  =    newUser
  :<|> getPublicKey
  :<|> getPublicKeyInfo
  :<|> listBusinesses

appHandlers :: (SCSApp context err, HasScryptParams context) => ServerT ServerAPI (AC.AppM context err)
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
basicAuthServerContext :: AC.SCSContext -> Servant.Context '[BasicAuthCheck User]
basicAuthServerContext context = authCheck context :. EmptyContext

minPubKeySize :: AC.Bit
minPubKeySize = AC.Bit 2048 -- 256 Bytes

type SCSApp context err =
  ( AsServiceError err
  , AsSqlError err
  , HasEnvType context
  , HasConnPool context
  )


addPublicKey :: SCSApp context err => User -> PEM_RSAPubKey -> AC.AppM context err KeyID
addPublicKey user pemKey@(PEMString pemStr) = do
  somePubKey <- liftIO $ readPublicKey pemStr
  either (throwing _ServiceError)
         (AC.runDb . BQ.addPublicKey user)
         (checkPubKey somePubKey pemKey)

checkPubKey :: SomePublicKey -> PEM_RSAPubKey-> Either ServiceError RSAPubKey
checkPubKey spKey pemKey =
  maybe (Left $ InvalidRSAKey pemKey)
  (\pubKey ->
    let keySize = rsaSize pubKey in
    -- rsaSize returns size in bytes
    if (AC.Bit $ keySize * 8) < minPubKeySize
      then Left $ InvalidRSAKeySize (Expected minPubKeySize) (Received $ AC.Bit keySize)
      else Right pubKey
  )
  (toPublicKey spKey)

newUser ::  (SCSApp context err, HasScryptParams context)=> NewUser -> AC.AppM context err UserID
newUser = AC.runDb . BQ.newUser

getPublicKey ::  SCSApp context err => KeyID -> AC.AppM context err PEM_RSAPubKey
getPublicKey = AC.runDb . BQ.getPublicKey

getPublicKeyInfo ::  SCSApp context err => KeyID -> AC.AppM context err KeyInfo
getPublicKeyInfo = AC.runDb . BQ.getPublicKeyInfo

-- PSUEDO:
-- In BeamQueries, implement a function getLabelIDState :: LabelEPCUrn -> IO (_labelID, State)
-- use readLabelEPC in EPC.hs to do it.
-- SELECT * FROM Labels WHERE _labelGs1CompanyPrefix=gs1CompanyPrefix AND _labelType=type AND ...

-- PSUEDO:
-- Use getLabelIDState
epcState :: User ->  LabelEPCUrn -> AC.AppM context err EPCState
epcState _user _str = U.notImplemented

-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
-- PSEUDO:
-- (labelID, _) <- getLabelIDState
-- wholeEvents <- select * from events, dwhats, dwhy, dwhen where _whatItemID=labelID AND _eventID=_whatEventID AND _eventID=_whenEventID AND _eventID=_whyEventID ORDER BY _eventTime;
-- return map constructEvent wholeEvents
listEvents ::  SCSApp context err => User ->  LabelEPCUrn -> AC.AppM context err [Ev.Event]
listEvents _user = either throwParseError (AC.runDb . BQ.listEvents) . urn2LabelEPC . unLabelEPCUrn

-- given an event ID, list all the users associated with that event
-- this can be used to make sure everything is signed
-- PSEUDO:
-- SELECT event.userID, userID1, userID2 FROM Events, BizTransactions WHERE Events._eventID=eventID AND BizTransactionsEventId=Events._eventID;
-- implement a function constructEvent :: WholeEvent -> Event

-- Look into usereventsT and tie that back to the user
-- the function getUser/selectUser might be helpful
eventUserList :: SCSApp context err => User -> EventId -> AC.AppM context err [(User, Bool)]
eventUserList _user = AC.runDb . BQ.eventUserSignedList

listContacts :: SCSApp context err => User -> AC.AppM context err [User]
listContacts = AC.runDb . BQ.listContacts


addContact :: SCSApp context err => User -> UserID -> AC.AppM context err Bool
addContact user userId = AC.runDb $ BQ.addContact user userId

removeContact :: SCSApp context err => User -> UserID -> AC.AppM context err Bool
removeContact user userId = AC.runDb $ BQ.removeContact user userId

contactsAdd :: SCSApp context err => User -> UserID -> AC.AppM context err Bool
contactsAdd user = AC.runDb . BQ.addContact user

contactsRemove :: SCSApp context err => User -> UserID -> AC.AppM context err Bool
contactsRemove user = AC.runDb . BQ.removeContact user

-- Given a search term, search the users contacts for a user matching
-- that term
-- might want to use reg-ex features of postgres10 here:
-- PSEUDO:
-- SELECT user2, firstName, lastName FROM Contacts, Users WHERE user1 LIKE *term* AND user2=Users.id UNION SELECT user1, firstName, lastName FROM Contacts, Users WHERE user2 = ? AND user1=Users.id;" (uid, uid)
--
contactsSearch :: User -> String -> AC.AppM context err [User]
contactsSearch _user _term = U.notImplemented


userSearch :: User -> String -> AC.AppM context err [User]
-- userSearch user term = liftIO $ Storage.userSearch user term
userSearch _user _term = error "Storage module not implemented"

-- select * from Business;
listBusinesses :: SCSApp context err => AC.AppM context err [Business]
listBusinesses = AC.runDb $ fmap QU.storageToModelBusiness <$> BQ.listBusinesses
-- ^ one fmap for Functor AppM, one for Functor []

-- |List events that a particular user was/is involved with
-- use BizTransactions and events (createdby) tables
eventList :: SCSApp context err => User -> UserID -> AC.AppM context err [Ev.Event]
eventList _user = AC.runDb . BQ.eventsByUser

makeDigest :: Digest -> IO (Maybe EVPDigest.Digest)
makeDigest = EVPDigest.getDigestByName . map toLower . show


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

eventSign :: SCSApp context err => User -> SignedEvent -> AC.AppM context err SB.PrimaryKeyType
eventSign _user (SignedEvent eventID keyID (Signature sigStr) digest') = AC.runDb $ do
  event <- BQ.getEventJSON eventID
  rsaPublicKey <- BQ.getPublicKey keyID
  sigBS <- BS64.decode (BSC.pack sigStr) <%?> review _InvalidSignature
  let (PEMString keyStr) = rsaPublicKey
  (pubKey :: RSAPubKey) <- liftIO (toPublicKey <$> readPublicKey keyStr) <!?> review _InvalidRSAKeyInDB (pack keyStr)
  let eventBS = QU.eventTxtToBS event
  digest <- liftIO (makeDigest digest') <!?> review _InvalidDigest digest'
  verifyStatus <- liftIO $ verifyBS digest sigBS pubKey eventBS
  if verifyStatus == VerifySuccess
    then BQ.insertSignature eventID keyID (Signature sigStr) digest'
    else throwAppError $ InvalidSignature sigStr

-- | A function to tie a user to an event
-- Populates the ``UserEvents`` table
addUserToEvent :: SCSApp context err => User -> UserID -> EventId -> AC.AppM context err ()
addUserToEvent (User loggedInUserId _ _) anotherUserId eventId =
    AC.runDb $ BQ.addUserToEvent (AC.EventOwner loggedInUserId) (AC.SigningUser anotherUserId) eventId

-- eventSign user signedEvent = error "Storage module not implemented"
-- eventSign user signedEvent = do
--   result <- liftIO $ runExceptT $ Storage.eventSign user signedEvent
--   case result of
--     Left SE_NeedMoreSignatures -> return False
--     Left e -> throwError err400 { errBody = LBSC8.pack $ show e }
--     Right () -> return True

-- do we need this?
--
eventHashed :: User -> EventId -> AC.AppM context err HashedEvent
eventHashed _user _eventId = error "not implemented yet"
-- return (HashedEvent eventID (EventHash "Blob"))

{-
eventHashed user eventID = do
  mHash <- liftIO $ Storage.eventHashed user eventID
  case mHash of
    Nothing -> throwError err404 { errBody = "Unknown eventID" }
    Just i -> return i
-}


insertObjectEvent :: SCSApp context err => User -> ObjectEvent -> AC.AppM context err Ev.Event
insertObjectEvent user ob = AC.runDb $ BQ.insertObjectEvent user ob

insertAggEvent :: SCSApp context err => User -> AggregationEvent -> AC.AppM context err Ev.Event
insertAggEvent user ev = AC.runDb $ BQ.insertAggEvent user ev

insertTransactEvent :: SCSApp context err => User -> TransactionEvent -> AC.AppM context err Ev.Event
insertTransactEvent user ev = AC.runDb $ BQ.insertTransactEvent user ev

insertTransfEvent :: SCSApp context err => User -> TransformationEvent -> AC.AppM context err Ev.Event
insertTransfEvent user ev = AC.runDb $ BQ.insertTransfEvent user ev


eventInfo :: SCSApp context err => User -> EventId -> AC.AppM context err (Maybe Ev.Event)
eventInfo _user = AC.runDb . QU.findEvent . SB.EventId . unEventId

--eventHash :: EventId -> AC.AppM context err SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
