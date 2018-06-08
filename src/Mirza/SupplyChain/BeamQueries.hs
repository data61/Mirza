{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | This module is incomplete as of yet.
-- Functions in the `service` module use the database functions defined here
module Mirza.SupplyChain.BeamQueries where



import           Mirza.SupplyChain.ErrorUtils             (getSqlErrorCode,
                                                           throwAppError,
                                                           throwBackendError,
                                                           toServerError)
import qualified Mirza.SupplyChain.MigrateUtils           as MU
import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.StorageBeam            as SB
import           Mirza.SupplyChain.Types                  hiding (KeyInfo (..),
                                                           NewUser (..),
                                                           User (userId),
                                                           UserID)
import qualified Mirza.SupplyChain.Types                  as ST

import           Data.GS1.DWhat                           (AggregationDWhat (..),
                                                           DWhat (..),
                                                           InputEPC (..),
                                                           LabelEPC (..),
                                                           ObjectDWhat (..),
                                                           OutputEPC (..),
                                                           TransactionDWhat (..),
                                                           TransformationDWhat (..),
                                                           unParentLabel)
import qualified Data.GS1.Event                           as Ev
import qualified Data.GS1.EventId                         as EvId

import           Control.Monad.Except                     (MonadError,
                                                           throwError)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Crypto.Scrypt                            as Scrypt
import           Data.Maybe                               (catMaybes)
import qualified Data.Text                                as T
import           Data.Text.Encoding
import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (..),
                                                           constraintViolation)
import           Database.PostgreSQL.Simple.Internal      (SqlError (..))
import           OpenSSL.PEM                              (writePublicKey)
import           OpenSSL.RSA                              (RSAPubKey)

import           Control.Lens                             (view, (^?), _2)


-- Basic Auth check using Scrypt hashes.
-- TODO: How safe is this to timing attacks? Can we tell which emails are in the
-- system easily?
authCheck :: (AsServiceError err, HasScryptParams context) =>  EmailAddress -> Password -> DB context err (Maybe ST.User)
authCheck e@(EmailAddress email) (Password password) = do
  r <- pg $ runSelectReturningList $ select $ do
        user <- all_ (SB._users SB.supplyChainDb)
        guard_ (SB.email_address user  ==. val_ email)
        pure user
  params <- view $ _2 . scryptParams
  case r of
    [user] ->
        case Scrypt.verifyPass params (Scrypt.Pass password)
              (Scrypt.EncryptedPass $ SB.password_hash user)
        of
          (False, _     ) -> throwAppError $ AuthFailed (EmailAddress email)
          (True, Nothing) -> pure $ Just (userTableToModel user)
          (True, Just (Scrypt.EncryptedPass password')) -> do
            _ <- pg $ runUpdate $ update (SB._users SB.supplyChainDb)
                    (\u -> [SB.password_hash u <-. val_ password'])
                    (\u -> SB.user_id u ==. val_ (SB.user_id user))
            pure $ Just (userTableToModel user)
    [] -> throwAppError $ EmailNotFound e
    _  -> throwBackendError r -- multiple elements




















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
