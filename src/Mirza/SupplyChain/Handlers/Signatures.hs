{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Mirza.SupplyChain.Handlers.Signatures
  (
    addUserToEvent
  , eventSign, getEventJSON, makeDigest, insertSignature, eventHashed
  ) where




import           Mirza.SupplyChain.Handlers.Common
import           Mirza.SupplyChain.Handlers.EventRegistration (hasUserCreatedEvent,
                                                               insertUserEvent)

import           Mirza.SupplyChain.ErrorUtils                 (throwAppError,
                                                               throwBackendError)
import qualified Mirza.SupplyChain.QueryUtils                 as QU
import qualified Mirza.SupplyChain.StorageBeam                as SB
import           Mirza.SupplyChain.Types                      hiding
                                                               (KeyInfo (..),
                                                               NewUser (..),
                                                               User (userId),
                                                               UserID)
import qualified Mirza.SupplyChain.Types                      as ST

import qualified Data.GS1.EventId                             as EvId

import           Database.Beam                                as B
import           Database.Beam.Backend.SQL.BeamExtensions

import qualified OpenSSL.EVP.Digest                           as EVPDigest
import           OpenSSL.EVP.PKey                             (toPublicKey)
import           OpenSSL.EVP.Verify                           (VerifyStatus (..),
                                                               verifyBS)
import           OpenSSL.PEM                                  (readPublicKey)
import           OpenSSL.RSA                                  (RSAPubKey)

import           Control.Lens                                 hiding ((.=))
import           Control.Monad.Error.Hoist                    ((<!?>), (<%?>))

import qualified Data.ByteString.Base64                       as BS64
import qualified Data.ByteString.Char8                        as BSC
import           Data.Char                                    (toLower)
import           Data.Text                                    (pack)
import qualified Data.Text                                    as T

-- | A function to tie a user to an event
-- Populates the ``UserEvents`` table
addUserToEvent :: SCSApp context err => ST.User -> ST.UserID -> EvId.EventId -> AppM context err ()
addUserToEvent (User loggedInUserId _ _) anotherUserId eventId =
    runDb $ addUserToEventQuery (EventOwner loggedInUserId) (SigningUser anotherUserId) eventId

addUserToEventQuery :: AsServiceError err => EventOwner -> SigningUser -> EvId.EventId -> DB context err ()
addUserToEventQuery (EventOwner lUserId@(ST.UserID loggedInUserId))
                (SigningUser (ST.UserID otherUserId))
                evId@(EvId.EventId eventId) = do
  userCreatedEvent <- hasUserCreatedEvent lUserId evId
  if userCreatedEvent
    then insertUserEvent
            (SB.EventId eventId)
            (SB.UserId loggedInUserId)
            (SB.UserId otherUserId)
            False Nothing
    else throwing _EventPermissionDenied (lUserId, evId)

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

eventSign :: SCSApp context err => ST.User -> SignedEvent -> AppM context err SB.PrimaryKeyType
eventSign _user (SignedEvent eventID keyID (Signature sigStr) digest') = runDb $ do
  event <- getEventJSON eventID
  rsaPublicKey <- getPublicKey keyID
  sigBS <- BS64.decode (BSC.pack sigStr) <%?> review _InvalidSignature
  let (PEMString keyStr) = rsaPublicKey
  (pubKey :: RSAPubKey) <- liftIO (toPublicKey <$> readPublicKey keyStr) <!?> review _InvalidRSAKeyInDB (pack keyStr)
  let eventBS = QU.eventTxtToBS event
  digest <- liftIO (makeDigest digest') <!?> review _InvalidDigest digest'
  verifyStatus <- liftIO $ verifyBS digest sigBS pubKey eventBS
  if verifyStatus == VerifySuccess
    then insertSignature eventID keyID (Signature sigStr) digest'
    else throwAppError $ InvalidSignature sigStr

-- TODO: Should this return Text or a JSON value?
getEventJSON :: AsServiceError err => EvId.EventId -> DB context err T.Text
getEventJSON eventID = do
  r <- pg $ runSelectReturningList $ select $ do
    allEvents <- all_ (SB._events SB.supplyChainDb)
    guard_ ((SB.event_id allEvents) ==. val_ (EvId.unEventId eventID))
    pure (SB.json_event allEvents)
  case r of
    [jsonEvent] -> return jsonEvent
    _           -> throwing _InvalidEventID eventID

getPublicKey :: AsServiceError err =>  KeyID -> DB context err PEM_RSAPubKey
getPublicKey (KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure (SB.pem_str allKeys)
  case r of
    [k] -> return $ PEMString $ T.unpack k
    _   -> throwing _InvalidKeyID . KeyID $ keyId

makeDigest :: Digest -> IO (Maybe EVPDigest.Digest)
makeDigest = EVPDigest.getDigestByName . map toLower . show


insertSignature
  :: (AsServiceError err) =>
     EvId.EventId
     -> KeyID
     -> Signature
     -> Digest
     -> DB environmentUnused err SB.PrimaryKeyType

insertSignature eId kId (Signature sig) digest = do
  sigId <- QU.generatePk
  timestamp <- QU.generateTimeStamp
  r <- pg $ runInsertReturningList (SB._signatures SB.supplyChainDb) $
        insertValues
        [(SB.Signature sigId) (SB.EventId $ EvId.unEventId eId)
         (SB.KeyId $ unKeyID kId) (BSC.pack sig)
          (BSC.pack $ show $ digest) timestamp]
  case r of
    [rowId] -> return ( SB.signature_id rowId)
    _       -> throwBackendError "Failed to add signature"



-- do we need this?
--
eventHashed :: ST.User -> EvId.EventId -> AppM context err HashedEvent
eventHashed _user _eventId = error "not implemented yet"
-- return (HashedEvent eventID (EventHash "Blob"))

{-
eventHashed user eventID = do
  mHash <- liftIO $ Storage.eventHashed user eventID
  case mHash of
    Nothing -> throwError err404 { errBody = "Unknown eventID" }
    Just i -> return i
-}
