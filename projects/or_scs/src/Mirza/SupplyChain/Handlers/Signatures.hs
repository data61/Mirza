{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Mirza.SupplyChain.Handlers.Signatures
  ( eventSign, getEventBS, insertSignature
  ) where

import           Mirza.Common.Time
import           Mirza.Common.Types                       (ORKeyId)
import           Mirza.Common.Utils

import           Mirza.SupplyChain.Database.Schema        as Schema
import           Mirza.SupplyChain.Handlers.Queries       (eventInfoQuery)
import           Mirza.SupplyChain.Types

import qualified Data.GS1.EventId                         as EvId

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres                   (PgJSON (..))


import           Crypto.JOSE                              (Alg (..), AsError,
                                                           CompactJWS,
                                                           JWSHeader,
                                                           ValidationSettings,
                                                           defaultValidationSettings,
                                                           validationSettingsAlgorithms,
                                                           verifyJWS)
import           Data.ByteString                          (ByteString)

import           Control.Lens                             ((&), (.~))

import           Mirza.OrgRegistry.Client.Servant    (getPublicKey)

scsJWSValidationSettings :: ValidationSettings
scsJWSValidationSettings = defaultValidationSettings
    & validationSettingsAlgorithms .~ [RS256,RS384,RS512,PS256,PS384,PS512]

eventSign :: (Member context '[HasDB, HasORClientEnv],
              Member err     '[AsError, AsServantError, AsSqlError, AsServiceError])
          => SignedEvent
          -> AppM context err EventInfo
eventSign (SignedEvent eventId keyId sig) = do
  jwk <- runClientFunc $ getPublicKey keyId
  runDb $ do
    eventBS <- getEventBS eventId
    event' <- verifyJWS scsJWSValidationSettings jwk sig
    if eventBS == event'
      then do
        _ <- insertSignature eventId keyId sig
        eventInfoQuery eventId
      else throwing _SigVerificationFailure (show sig) -- TODO: This should be more than show

getEventBS  :: Member err '[AsServiceError]
            => EvId.EventId
            -> DB context err ByteString
getEventBS eventId = do
  r <- pg $ runSelectReturningList $ select $ do
    allEvents <- all_ (Schema._events Schema.supplyChainDb)
    guard_ ((Schema.event_id allEvents) ==. val_ (EvId.unEventId eventId))
    pure (Schema.event_to_sign allEvents)
  case r of
    [eventBS] -> pure eventBS
    _         -> throwing _InvalidEventId eventId

insertSignature :: Member err '[AsServiceError]
                => EvId.EventId
                -> ORKeyId
                -> CompactJWS JWSHeader
                -> DB environmentUnused err PrimaryKeyType
insertSignature eId kId sig = do
  sigId <- newUUID
  timestamp <- generateTimestamp
  r <- pg $ runInsertReturningList (Schema._signatures Schema.supplyChainDb) $
        insertValues
        [Schema.Signature Nothing sigId (Schema.EventId $ EvId.unEventId eId)
         (ORKeyId $ getORKeyId kId) (PgJSON sig) (toDbTimestamp timestamp)]
  case r of
    [rowId] ->
      -- updateUserEventSignature userId eId True
      -- TODO: Update the list of required signatures
      pure ( Schema.signature_id rowId)
    _       -> throwing _BackendErr "Failed to add signature"

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
--         pure $ C.BlockchainPackage (EventHash plainHash) signatures
--     else throwError SE_BlockchainSendFailed
-- --TODO - Implement me
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

