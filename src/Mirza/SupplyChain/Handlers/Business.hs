{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Business
  (
    getPublicKey, getPublicKeyInfo
  , revokePublicKey, isKeyRevoked
  , checkPubKey
  , addPublicKey
  , listBusinesses
  , getKeyById
  ) where



import           Mirza.SupplyChain.Handlers.Common

import           Mirza.BusinessRegistry.Types             as BRT
import           Mirza.Common.Utils
import qualified Mirza.SupplyChain.QueryUtils             as QU
import qualified Mirza.SupplyChain.StorageBeam            as SB
import           Mirza.SupplyChain.Types                  hiding (KeyInfo (..),
                                                           NewUser (..),
                                                           User (userId),
                                                           UserID)
import qualified Mirza.SupplyChain.Types                  as ST

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           OpenSSL.EVP.PKey                         (SomePublicKey,
                                                           toPublicKey)
import           OpenSSL.PEM                              (readPublicKey,
                                                           writePublicKey)
import           OpenSSL.RSA                              (RSAPubKey, rsaSize)

import           Control.Monad                            (unless, when)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Data.Text                                as T

import           Data.Time.Clock                          (UTCTime,
                                                           getCurrentTime)
import           Data.Time.LocalTime                      (utc, utcToLocalTime)




minPubKeySize :: BRT.Bit
minPubKeySize = BRT.Bit 2048 -- 256 Bytes


getPublicKey ::  SCSApp context err => KeyID -> AppM context err BRT.PEM_RSAPubKey
getPublicKey = runDb . getPublicKeyQuery

getPublicKeyQuery :: AsServiceError err => KeyID -> DB context err BRT.PEM_RSAPubKey
getPublicKeyQuery (KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure (SB.pem_str allKeys)
  case r of
    [k] -> return $ PEMString $ T.unpack k
    _   -> throwing _InvalidKeyID . KeyID $ keyId


getPublicKeyInfo ::  SCSApp context err => KeyID -> AppM context err BRT.KeyInfoResponse
getPublicKeyInfo = runDb . getPublicKeyInfoQuery

getPublicKeyInfoQuery :: AsServiceError err => KeyID -> DB context err BRT.KeyInfoResponse
getPublicKeyInfoQuery (KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure allKeys
  currTime <- liftIO getCurrentTime
  case r of
    [(SB.Key _ (SB.UserId uId) _  creationTime revocationTime mExpTime)] ->
       return $ BRT.KeyInfoResponse (ST.UserID uId)
                (QU.onLocalTime CreationTime creationTime)
                (QU.onLocalTime RevocationTime <$> revocationTime)
                (getKeyState currTime
                    (QU.onLocalTime RevocationTime <$> revocationTime)
                    ((QU.onLocalTime ExpirationTime <$> mExpTime))
                )
                (QU.onLocalTime ExpirationTime <$> mExpTime)
    _ -> throwing _InvalidKeyID . KeyID $ keyId



-- select * from Business;
listBusinesses :: SCSApp context err => AppM context err [BRT.BusinessResponse]
listBusinesses = runDb $ fmap QU.storageToModelBusiness <$> listBusinessesQuery
-- ^ one fmap for Functor AppM, one for Functor []

-- TODO: Write tests
listBusinessesQuery :: DB context err [BRT.BusinessResponse]
listBusinessesQuery = do
  pg $ runSelectReturningList $ select $
      all_ (SB._businesses SB.supplyChainDb)



addPublicKey :: SCSApp context err => ST.User
             -> BRT.PEM_RSAPubKey
             -> Maybe BRT.ExpirationTime
             -> AppM context err KeyID
addPublicKey user pemKey@(BRT.PEM_RSAPubKey pemStr) mExp = do
  somePubKey <- liftIO $ readPublicKey pemStr
  either (throwing _ServiceError)
         (runDb . addPublicKeyQuery user mExp)
         (checkPubKey somePubKey pemKey)



checkPubKey :: SomePublicKey -> BRT.PEM_RSAPubKey-> Either ServiceError RSAPubKey
checkPubKey spKey pemKey =
  maybe (Left $ InvalidRSAKey pemKey)
  (\pubKey ->
    let keySizeBits = Bit $ rsaSize pubKey * 8 in
    -- rsaSize returns size in bytes
    if keySizeBits < minPubKeySize
      then Left $ InvalidRSAKeySize (Expected minPubKeySize) (Received keySizeBits)
      else Right pubKey
  )
  (toPublicKey spKey)


addPublicKeyQuery :: AsServiceError err => ST.User
                  -> Maybe BRT.ExpirationTime
                  -> RSAPubKey
                  -> DB context err KeyID
addPublicKeyQuery (User (ST.UserID uid) _ _) expTime rsaPubKey = do
  keyId <- newUUID
  timeStamp <- QU.generateTimestamp
  keyStr <- liftIO $ writePublicKey rsaPubKey
  r <- pg $ runInsertReturningList (SB._keys SB.supplyChainDb) $
        insertValues
        [ SB.Key keyId (SB.UserId uid) (T.pack keyStr)
            timeStamp Nothing ((utcToLocalTime utc) . unExpirationTime <$> expTime)
        ]
  case r of
    [rowId] -> return (KeyID $ SB.key_id rowId)
    _       -> throwing _BackendErr "Failed to add public key"



revokePublicKey :: SCSApp context err => ST.User -> KeyID -> AppM context err UTCTime
revokePublicKey (ST.User userId _ _) keyId =
    runDb $ revokePublicKeyQuery userId keyId

isKeyRevoked :: AsServiceError err => KeyID -> DB context err Bool
isKeyRevoked k = do
  keyInfo <- getPublicKeyInfoQuery k
  return $ BRT.keyState keyInfo == Revoked

revokePublicKeyQuery :: AsServiceError err => ST.UserID -> KeyID -> DB context err UTCTime
revokePublicKeyQuery userId k@(KeyID keyId) = do
  userOwnsKey <- doesUserOwnKey userId k
  unless userOwnsKey $ throwing_ BRT._UnauthorisedKeyAccess
  keyRevoked <- isKeyRevoked k
  when keyRevoked $ throwing_ BRT._KeyAlreadyRevoked
  timeStamp <- QU.generateTimestamp
  _r <- pg $ runUpdate $ update
                (SB._keys SB.supplyChainDb)
                (\key -> [SB.revocation_time key <-. val_ (Just timeStamp)])
                (\key -> SB.key_id key ==. (val_ keyId))
  return $ QU.onLocalTime id timeStamp



-- Helper functions

doesUserOwnKey :: AsServiceError err => ST.UserID -> ST.KeyID -> DB context err Bool
doesUserOwnKey (ST.UserID userId) (ST.KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
          key <- all_ (SB._keys SB.supplyChainDb)
          guard_ (SB.key_id key ==. val_ keyId)
          guard_ (val_ (SB.UserId userId) ==. (SB.key_user_id key))
          pure key
  return $ case r of
    [_key] -> True
    _      -> False

getKeyById :: ST.KeyID -> DB context err (Maybe SB.Key)
getKeyById (ST.KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
          key <- all_ (SB._keys SB.supplyChainDb)
          guard_ (SB.key_id key ==. val_ keyId)
          pure key
  case r of
    [key] -> return $ Just key
    _     -> return Nothing


getKeyState :: UTCTime
            -> Maybe BRT.RevocationTime
            -> Maybe BRT.ExpirationTime
            -> BRT.KeyState
-- order of precedence - Revoked > Expired
getKeyState currTime (Just (BRT.RevocationTime rTime)) (Just (BRT.ExpirationTime eTime))
  | currTime > rTime = BRT.Revoked
  | currTime > eTime = BRT.Expired
  | otherwise        = BRT.InEffect
getKeyState currTime Nothing (Just (BRT.ExpirationTime eTime))
  | currTime > eTime = BRT.Expired
  | otherwise        = BRT.InEffect
getKeyState currTime (Just (BRT.RevocationTime rTime)) Nothing
  | currTime > rTime = BRT.Revoked
  | otherwise        = BRT.InEffect
getKeyState _ Nothing Nothing = BRT.InEffect

