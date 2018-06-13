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
import           Mirza.SupplyChain.Types       hiding (NewUser (..),
                                                User (userId))
import qualified Mirza.SupplyChain.Types       as MT
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
import           Data.Time.Clock               (UTCTime)
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
authCheck :: SCSContext -> BasicAuthCheck MT.User
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

privateServer :: (SCSApp context err) => ServerT ProtectedAPI (AppM context err)
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
  :<|> revokePublicKey

publicServer :: (SCSApp context err, HasScryptParams context) => ServerT PublicAPI (AppM context err)
publicServer
  =    newUser
  :<|> getPublicKey
  :<|> getPublicKeyInfo
  :<|> listBusinesses

appHandlers :: (SCSApp context err, HasScryptParams context) => ServerT ServerAPI (AppM context err)
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
basicAuthServerContext :: SCSContext -> Servant.Context '[BasicAuthCheck MT.User]
basicAuthServerContext context = authCheck context :. EmptyContext

minPubKeySize :: Bit
minPubKeySize = Bit 2048 -- 256 Bytes

type SCSApp context err =
  ( AsServiceError err
  , AsSqlError err
  , HasEnvType context
  , HasConnPool context
  )


addPublicKey :: SCSApp context err => MT.User
             -> MT.PEM_RSAPubKey
             -> Maybe MT.ExpirationTime
             -> MT.AppM context err MT.KeyID
addPublicKey user pemKey@(MT.PEMString pemStr) mExp = do
  somePubKey <- liftIO $ readPublicKey pemStr
  either (throwing _ServiceError)
         (runDb . BQ.addPublicKey user mExp)
         (checkPubKey somePubKey pemKey)

revokePublicKey :: SCSApp context err => MT.User -> MT.KeyID -> MT.AppM context err UTCTime
revokePublicKey (MT.User userId _ _) keyId =
    runDb $ BQ.revokePublicKey userId keyId

checkPubKey :: SomePublicKey -> MT.PEM_RSAPubKey-> Either ServiceError RSAPubKey
checkPubKey spKey pemKey =
  maybe (Left $ InvalidRSAKey pemKey)
  (\pubKey ->
    let keySize = rsaSize pubKey in
    -- rsaSize returns size in bytes
    if (Bit $ keySize * 8) < minPubKeySize
      then Left $ InvalidRSAKeySize (Expected minPubKeySize) (Received $ Bit keySize)
      else Right pubKey
  )
  (toPublicKey spKey)

newUser ::  (SCSApp context err, HasScryptParams context)=> MT.NewUser -> AppM context err UserID
newUser = runDb . BQ.newUser

getPublicKey ::  SCSApp context err => KeyID -> AppM context err PEM_RSAPubKey
getPublicKey = runDb . BQ.getPublicKey

getPublicKeyInfo ::  SCSApp context err => KeyID -> AppM context err KeyInfo
getPublicKeyInfo = runDb . BQ.getPublicKeyInfo

-- PSUEDO:
-- In BeamQueries, implement a function getLabelIDState :: LabelEPCUrn -> IO (_labelID, State)
-- use readLabelEPC in EPC.hs to do it.
-- SELECT * FROM Labels WHERE _labelGs1CompanyPrefix=gs1CompanyPrefix AND _labelType=type AND ...

-- PSUEDO:
-- Use getLabelIDState
epcState :: MT.User ->  LabelEPCUrn -> AppM context err EPCState
epcState _user _str = U.notImplemented

-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
-- PSEUDO:
-- (labelID, _) <- getLabelIDState
-- wholeEvents <- select * from events, dwhats, dwhy, dwhen where _whatItemID=labelID AND _eventID=_whatEventID AND _eventID=_whenEventID AND _eventID=_whyEventID ORDER BY _eventTime;
-- return map constructEvent wholeEvents
listEvents ::  SCSApp context err => MT.User ->  LabelEPCUrn -> AppM context err [Ev.Event]
listEvents _user = either throwParseError (runDb . BQ.listEvents) . urn2LabelEPC . unLabelEPCUrn

-- given an event ID, list all the users associated with that event
-- this can be used to make sure everything is signed
-- PSEUDO:
-- SELECT event.userID, userID1, userID2 FROM Events, BizTransactions WHERE Events._eventID=eventID AND BizTransactionsEventId=Events._eventID;
-- implement a function constructEvent :: WholeEvent -> Event

-- Look into usereventsT and tie that back to the user
-- the function getUser/getUserById might be helpful
eventUserList :: SCSApp context err => MT.User -> EventId -> MT.AppM context err [(MT.User, Bool)]
eventUserList _user = runDb . BQ.eventUserSignedList

listContacts :: SCSApp context err => MT.User -> AppM context err [MT.User]
listContacts = runDb . BQ.listContacts


addContact :: SCSApp context err => MT.User -> UserID -> AppM context err Bool
addContact user userId = runDb $ BQ.addContact user userId

removeContact :: SCSApp context err => MT.User -> UserID -> AppM context err Bool
removeContact user userId = runDb $ BQ.removeContact user userId

contactsAdd :: SCSApp context err => MT.User -> UserID -> AppM context err Bool
contactsAdd user = runDb . BQ.addContact user

contactsRemove :: SCSApp context err => MT.User -> UserID -> AppM context err Bool
contactsRemove user = runDb . BQ.removeContact user

-- Given a search term, search the users contacts for a user matching
-- that term
-- might want to use reg-ex features of postgres10 here:
-- PSEUDO:
-- SELECT user2, firstName, lastName FROM Contacts, Users WHERE user1 LIKE *term* AND user2=Users.id UNION SELECT user1, firstName, lastName FROM Contacts, Users WHERE user2 = ? AND user1=Users.id;" (uid, uid)
--
contactsSearch :: MT.User -> String -> AppM context err [MT.User]
contactsSearch _user _term = U.notImplemented


userSearch :: MT.User -> String -> AppM context err [MT.User]
-- userSearch user term = liftIO $ Storage.userSearch user term
userSearch _user _term = error "Storage module not implemented"

-- select * from Business;
listBusinesses :: SCSApp context err => AppM context err [Business]
listBusinesses = runDb $ fmap QU.storageToModelBusiness <$> BQ.listBusinesses
-- ^ one fmap for Functor AppM, one for Functor []

-- |List events that a particular user was/is involved with
-- use BizTransactions and events (createdby) tables
eventList :: SCSApp context err => MT.User -> UserID -> AppM context err [Ev.Event]
eventList _user = runDb . BQ.eventsByUser

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

eventSign :: SCSApp context err => MT.User -> SignedEvent -> AppM context err SB.PrimaryKeyType
eventSign _user (SignedEvent eventID keyID (Signature sigStr) digest') = runDb $ do
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
addUserToEvent :: SCSApp context err => MT.User -> UserID -> EventId -> AppM context err ()
addUserToEvent (User loggedInUserId _ _) anotherUserId eventId =
    runDb $ BQ.addUserToEvent (EventOwner loggedInUserId) (SigningUser anotherUserId) eventId

-- eventSign user signedEvent = error "Storage module not implemented"
-- eventSign user signedEvent = do
--   result <- liftIO $ runExceptT $ Storage.eventSign user signedEvent
--   case result of
--     Left SE_NeedMoreSignatures -> return False
--     Left e -> throwError err400 { errBody = LBSC8.pack $ show e }
--     Right () -> return True

-- do we need this?
--
eventHashed :: MT.User -> EventId -> AppM context err HashedEvent
eventHashed _user _eventId = error "not implemented yet"
-- return (HashedEvent eventID (EventHash "Blob"))

{-
eventHashed user eventID = do
  mHash <- liftIO $ Storage.eventHashed user eventID
  case mHash of
    Nothing -> throwError err404 { errBody = "Unknown eventID" }
    Just i -> return i
-}


insertObjectEvent :: SCSApp context err => MT.User -> ObjectEvent -> AppM context err Ev.Event
insertObjectEvent user ob = runDb $ BQ.insertObjectEvent user ob

insertAggEvent :: SCSApp context err => MT.User -> AggregationEvent -> AppM context err Ev.Event
insertAggEvent user ev = runDb $ BQ.insertAggEvent user ev

insertTransactEvent :: SCSApp context err => MT.User -> TransactionEvent -> AppM context err Ev.Event
insertTransactEvent user ev = runDb $ BQ.insertTransactEvent user ev

insertTransfEvent :: SCSApp context err => MT.User -> TransformationEvent -> AppM context err Ev.Event
insertTransfEvent user ev = runDb $ BQ.insertTransfEvent user ev


eventInfo :: SCSApp context err => MT.User -> EventId -> AppM context err (Maybe Ev.Event)
eventInfo _user = runDb . QU.findEvent . SB.EventId . unEventId

--eventHash :: EventId -> AppM context err SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
