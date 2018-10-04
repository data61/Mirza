{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Mirza.SupplyChain.Handlers.Signatures
  (
    addUserToEvent
  , eventSign, getEventJSON, insertSignature, eventHashed
  ) where

import           Mirza.Common.Time
import           Mirza.Common.Types                           (BRKeyId)
import           Mirza.Common.Utils

-- import qualified Mirza.BusinessRegistry.Types                 as BT

import           Mirza.SupplyChain.Database.Schema            as Schema
import           Mirza.SupplyChain.Handlers.Common
import           Mirza.SupplyChain.Handlers.EventRegistration (hasUserCreatedEvent,
                                                               insertUserEvent)
import           Mirza.SupplyChain.Types                      hiding
                                                               (NewUser (..),
                                                               User (userId),
                                                               UserId)
import qualified Mirza.SupplyChain.Types                      as ST

import qualified Data.GS1.EventId                             as EvId

import           Database.Beam                                as B
import           Database.Beam.Backend.SQL.BeamExtensions


import           Crypto.JOSE                                  (verifyJWS', CompactJWS, JWSHeader, AsError)
import           Database.Beam.Postgres                       (PgJSON(..))

import qualified Data.ByteString.Char8                        as BSC
import qualified Data.ByteString                              as BS

import           Mirza.BusinessRegistry.Client.Servant        (getPublicKey)

-- | A function to tie a user to an event
-- Populates the ``UserEvents`` table
addUserToEvent :: SCSApp context err
               => ST.User
               -> ST.UserId
               -> EvId.EventId
               -> AppM context err ()
addUserToEvent (ST.User loggedInUserId _ _) anotherUserId eventId =
    runDb $ addUserToEventQuery (EventOwner loggedInUserId) (SigningUser anotherUserId) eventId

addUserToEventQuery :: AsServiceError err
                    => EventOwner
                    -> SigningUser
                    -> EvId.EventId
                    -> DB context err ()
addUserToEventQuery (EventOwner lUserId@(ST.UserId loggedInUserId))
                (SigningUser (ST.UserId otherUserId))
                evId@(EvId.EventId eventId) = do
  userCreatedEvent <- hasUserCreatedEvent lUserId evId
  if userCreatedEvent
    then insertUserEvent 
            (Schema.EventId eventId)
            (Schema.UserId loggedInUserId)
            (Schema.UserId otherUserId)
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

eventSign :: (HasBRClientEnv context, AsServantError err, AsError err, SCSApp context err)
          => ST.User
          -> SignedEvent
          -> AppM context err PrimaryKeyType
eventSign _user (SignedEvent eventId keyId sig digtype) = do
  jwk <- runClientFunc $ getPublicKey keyId
  -- sigBS <- BS64.decode (BSC.pack sigStr) <%?> review _Base64DecodeFailure
  -- digest <- liftIO (makeDigest digest') <!?> review _InvalidDigest digest'
  runDb $ do
    event <- getEventJSON eventId
    -- let eventBS = QU.eventTxtToBS event
    event' <- verifyJWS' jwk sig
    if event == event'
      then insertSignature eventId keyId sig digtype
      else throwing _SigVerificationFailure (show sig)

-- TODO: Should this return Text or a JSON value?
getEventJSON :: AsServiceError err => EvId.EventId -> DB context err BS.ByteString
getEventJSON eventId = do
  r <- pg $ runSelectReturningList $ select $ do
    allEvents <- all_ (Schema._events Schema.supplyChainDb)
    guard_ ((Schema.event_id allEvents) ==. val_ (EvId.unEventId eventId))
    pure (Schema.event_json allEvents)
  case r of
    [jsonEvent] -> return jsonEvent
    _           -> throwing _InvalidEventId eventId


-- makeDigest :: DigestType -> IO (Maybe EVPDigest.Digest)
-- makeDigest digest = withOpenSSL $ EVPDigest.getDigestByName . map toLower . show $ digest

insertSignature :: (AsServiceError err) => EvId.EventId
                -> BRKeyId
                -> CompactJWS JWSHeader
                -> DigestType
                -> DB environmentUnused err PrimaryKeyType

insertSignature eId kId sig digest = do
  sigId <- newUUID
  timestamp <- generateTimestamp
  r <- pg $ runInsertReturningList (Schema._signatures Schema.supplyChainDb) $
        insertValues
        [Schema.Signature sigId (Schema.EventId $ EvId.unEventId eId)
         (BRKeyId $ getBRKeyId kId) (PgJSON sig)
          (BSC.pack $ show digest) (toDbTimestamp timestamp)]
  case r of
    [rowId] -> return ( Schema.signature_id rowId)
    _       -> throwing _BackendErr "Failed to add signature"


-- do we need this?
eventHashed :: ST.User -> EvId.EventId -> AppM context err HashedEvent
eventHashed _user _eventId = error "not implemented yet"
-- return (HashedEvent eventId (EventHash "Blob"))

{-
eventHashed user eventId = do
  mHash <- liftIO $ Storage.eventHashed user eventId
  case mHash of
    Nothing -> throwError err404 { errBody = "Unknown eventId" }
    Just i -> return i
-}

-- BeamQueries residue

-- -- TODO - convert these below functions, and others in original file Storage.hs
-- -- TODO = use EventId or EventId ???
-- -- TODO = implement... there is no hash...
-- createBlockchainPackage ::  (MonadError SigError m, MonadIO m) => EventId -> m C.BlockchainPackage


-- createBlockchainPackage eventID = do
--   -- XXX do we want to explicitly check that the hash is signed before assuming the first one is not?
--   r <- liftIO $ query_ conn "SELECT hash, signedByUserID FROM Hashes WHERE eventID=? ORDER BY isSigned ASC;" $ Only eventID
--   if length r > 2
--     then
--       let (plainHash, userID) = head r
--           signatures = NonEmpty.fromList (map (\(s, u) -> ((Signature s), u)) (tail r))
--       in
--         return $ C.BlockchainPackage (EventHash plainHash) signatures
--     else throwError SE_BlockchainSendFailed
-- --TODO - Implement me
-- -- sendToBlockchain ::  (MonadError SigError m, MonadIO m) =>  C.BlockchainPackage -> m ()
-- sendToBlockchain :: Monad m => C.BlockchainPackage -> m ()
-- sendToBlockchain package = return () -- if it fails, raise SE_SEND_TO_BLOCKCHAIN_FAILED error.

-- checkSignature :: (MonadError SigError m, MonadIO m) => PEM_RSAPubKey -> ByteString.ByteString -> Signature -> m ()
-- checkSignature pubkey blob signature =
--   unless (C.verifySignature pubkey blob signature) $
--     throwError SE_InvalidSignature
-- -- TODO = use EventId or EventId
-- -- ready to send to blockchain when all the parties have signed
-- checkReadyForBlockchain :: (MonadError SigError m, MonadIO m) => Connection -> EventId -> m ()
-- checkReadyForBlockchain eventID = do
--   r <- liftIO $ query_ conn "SELECT COUNT(id) FROM UserEvents WHERE eventID=? AND hasSigned=FALSE;" $ Only eventID
--   case r of
--     [Only 0] -> pure ()
--     _ -> throwError SE_NeedMoreSignatures

