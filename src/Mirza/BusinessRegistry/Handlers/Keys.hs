{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.BusinessRegistry.Handlers.Keys
  (
    getPublicKey
  , getPublicKeyInfo
  , revokePublicKey
  , addPublicKey
  ) where


import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Types
import           Mirza.Common.Utils

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           Data.Text                                (pack, unpack)
import           Data.Time.Clock                          (UTCTime,
                                                           getCurrentTime)

import           OpenSSL.EVP.PKey                         (SomePublicKey,
                                                           toPublicKey)
import           OpenSSL.PEM                              (readPublicKey,
                                                           writePublicKey)
import           OpenSSL.RSA                              (RSAPubKey, rsaSize)



minPubKeySize :: Bit
minPubKeySize = Bit 2048

getPublicKey :: (BRApp context err, AsKeyError err) => KeyID -> AppM context err PEM_RSAPubKey
getPublicKey kid = do
  mpem <- runDb $ getPublicKeyQuery kid
  maybe (throwing _KeyNotFound kid) pure mpem

getPublicKeyQuery :: BRApp context err => KeyID -> DB context err (Maybe PEM_RSAPubKey)
getPublicKeyQuery (KeyID uuid) = fmap (fmap PEM_RSAPubKey) $ pg $ runSelectReturningOne $
  select $ do
    keys <- all_ (_keys businessRegistryDB)
    guard_ (primaryKey keys ==. val_ (KeyId uuid))
    pure (pem_str keys)

getPublicKeyInfo :: (BRApp context err, AsKeyError err) => KeyID -> AppM context err BT.KeyInfo
getPublicKeyInfo kid = do
  currTime <- liftIO getCurrentTime
  mkey <- runDb $ getPublicKeyInfoQuery kid
  maybe (throwing _KeyNotFound kid)
    (\(KeyT keyId keyUserId pemStr creation revocation expiration ) ->
      pure (KeyInfo (KeyID keyId) keyUserId
                    (getKeyState currTime
                      (onLocalTime RevocationTime <$> revocation)
                      (onLocalTime ExpirationTime <$> expiration)
                    )
                    (onLocalTime CreationTime creation)
                    (onLocalTime RevocationTime <$> revocation)
                    (onLocalTime ExpirationTime <$> expiration)
                    (PEM_RSAPubKey pemStr)
            )
    )
    mkey


getPublicKeyInfoQuery :: BRApp context err => KeyID -> DB context err (Maybe Key)
getPublicKeyInfoQuery (KeyID uuid) = pg $ runSelectReturningOne $
  select $ do
    keys <- all_ (_keys businessRegistryDB)
    guard_ (primaryKey keys ==. val_ (KeyId uuid))
    pure keys

addPublicKey :: (BRApp context err, AsKeyError err) => BT.AuthUser
             -> PEM_RSAPubKey
             -> Maybe ExpirationTime
             -> AppM context err KeyID
addPublicKey user pemKey@(PEM_RSAPubKey pemStr) mExp = do
  somePubKey <- liftIO $ readPublicKey (unpack pemStr) -- TODO: Catch exception from OpenSSL - any invalid PEM string causes exception
  rsaKey <- checkPubKey somePubKey pemKey              -- Input: "x"
  runDb $ addPublicKeyQuery user mExp rsaKey           -- Error: user error (error:0906D06C:PEM routines:PEM_read_bio:no start line)



checkPubKey :: (MonadError err m, AsKeyError err)
            => SomePublicKey -> PEM_RSAPubKey-> m RSAPubKey
checkPubKey spKey pemKey =
  maybe (throwing _InvalidRSAKey pemKey)
  (\pubKey ->
    let keySizeBits = Bit $ rsaSize pubKey * 8 in
    -- rsaSize returns size in bytes
    if keySizeBits < minPubKeySize
      then throwing _InvalidRSAKeySize (Expected minPubKeySize, Received keySizeBits)
      else pure pubKey
  )
  (toPublicKey spKey)


addPublicKeyQuery :: AsKeyError err => AuthUser
                  -> Maybe ExpirationTime
                  -> RSAPubKey
                  -> DB context err KeyID
addPublicKeyQuery (AuthUser uid) expTime rsaPubKey = do
  keyStr <- liftIO $ pack <$> writePublicKey rsaPubKey
  keyId <- newUUID
  timeStamp <- generateTimestamp
  ks <- pg $ runInsertReturningList (_keys businessRegistryDB) $
        insertValues
        [ KeyT keyId uid keyStr
            timeStamp Nothing (toLocalTime . unExpirationTime <$> expTime)
        ]
  case ks of
    [rowId] -> return (KeyID $ key_id rowId)
    _       -> throwing _PublicKeyInsertionError (map primaryKey ks)



revokePublicKey :: BRApp context err => BT.AuthUser -> KeyID -> AppM context err UTCTime
revokePublicKey = notImplemented


getKeyState :: UTCTime
            -> Maybe RevocationTime
            -> Maybe ExpirationTime
            -> KeyState
-- order of precedence - Revoked > Expired
getKeyState currTime (Just (RevocationTime rTime)) (Just (ExpirationTime eTime))
  | currTime > rTime = Revoked
  | currTime > eTime = Expired
  | otherwise        = InEffect
getKeyState currTime Nothing (Just (ExpirationTime eTime))
  | currTime > eTime = Expired
  | otherwise        = InEffect
getKeyState currTime (Just (RevocationTime rTime)) Nothing
  | currTime > rTime = Revoked
  | otherwise        = InEffect
getKeyState _ Nothing Nothing = InEffect

