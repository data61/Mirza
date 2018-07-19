{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.BusinessRegistry.Handlers.Keys
  (
    getPublicKey
  , getPublicKeyInfo
  , revokePublicKey
  , addPublicKey
  ) where


import           Mirza.BusinessRegistry.Database.Schema   as Schema
import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Types                       as CT
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

import           Control.Monad                            (unless, when)

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

getPublicKeyInfo :: (BRApp context err, AsKeyError err) => KeyID -> AppM context err BT.KeyInfoResponse
getPublicKeyInfo kid = do
  currTime <- liftIO getCurrentTime
  mkey <- runDb $ getPublicKeyInfoQuery kid
  maybe (throwing _KeyNotFound kid)
        (pure . keyToKeyInfo currTime)
        mkey

keyToKeyInfo :: UTCTime -> Schema.Key -> KeyInfoResponse
keyToKeyInfo currTime (Schema.KeyT keyId (Schema.UserId keyUserId) pemStr creation revocation expiration ) =
  (KeyInfoResponse (KeyID keyId) (CT.UserID keyUserId)
    (getKeyState
      (onLocalTime RevocationTime <$> revocation)
      (onLocalTime ExpirationTime <$> expiration)
    )
    (onLocalTime CreationTime creation)
    (onLocalTime RevocationTime <$> revocation)
    (onLocalTime ExpirationTime <$> expiration)
    (PEM_RSAPubKey pemStr)
  )
  where
    -- TODO: After migrating to JOSE, there should always be an expiration time.
    getKeyState :: Maybe RevocationTime
                -> Maybe ExpirationTime
                -> KeyState
    -- order of precedence - Revoked > Expired
    getKeyState (Just (RevocationTime rTime)) (Just (ExpirationTime eTime))
      | currTime >= rTime = Revoked
      | currTime >= eTime = Expired
      | otherwise        = InEffect
    getKeyState Nothing (Just (ExpirationTime eTime))
      | currTime >= eTime = Expired
      | otherwise        = InEffect
    getKeyState (Just (RevocationTime rTime)) Nothing
      | currTime >= rTime = Revoked
      | otherwise        = InEffect
    getKeyState Nothing Nothing = InEffect




getPublicKeyInfoQuery :: BRApp context err => KeyID -> DB context err (Maybe Schema.Key)
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
addPublicKeyQuery (AuthUser (CT.UserID uid)) expTime rsaPubKey = do
  keyStr <- liftIO $ pack <$> writePublicKey rsaPubKey
  keyId <- newUUID
  timeStamp <- generateTimestamp
  ks <- pg $ runInsertReturningList (_keys businessRegistryDB) $
        insertValues
        [ KeyT keyId (Schema.UserId uid) keyStr
            timeStamp Nothing (toLocalTime . unExpirationTime <$> expTime)
        ]
  case ks of
    [rowId] -> return (KeyID $ key_id rowId)
    _       -> throwing _PublicKeyInsertionError (map primaryKey ks)



revokePublicKey :: (BRApp context err, AsKeyError err) => BT.AuthUser -> KeyID -> AppM context err UTCTime
revokePublicKey (AuthUser uId) keyId =
    runDb $ revokePublicKeyQuery uId keyId

isKeyRevokedQuery :: (BRApp context err, AsKeyError err) => KeyID -> DB context err Bool
isKeyRevokedQuery kid = do
  currTime <- liftIO getCurrentTime
  mkeyInfo <- fmap (keyToKeyInfo currTime) <$> getPublicKeyInfoQuery kid
  maybe (throwing _KeyNotFound kid)
        (\ki -> pure $ keyState ki == Revoked)
        mkeyInfo

revokePublicKeyQuery :: (BRApp context err, AsKeyError err)
                     => CT.UserID -> CT.KeyID -> DB context err UTCTime
revokePublicKeyQuery uId k@(KeyID keyId) = do
  userOwnsKey <- doesUserOwnKeyQuery uId k
  unless userOwnsKey $ throwing_ _UnauthorisedKeyAccess
  keyRevoked <- isKeyRevokedQuery k
  when keyRevoked $ throwing_ _KeyAlreadyRevoked
  timeStamp <- generateTimestamp
  _r <- pg $ runUpdate $ update
                (_keys businessRegistryDB)
                (\key -> [revocation_time key <-. val_ (Just timeStamp)])
                (\key -> key_id key ==. (val_ keyId))
  return $ onLocalTime id timeStamp

doesUserOwnKeyQuery :: AsKeyError err => CT.UserID -> KeyID -> DB context err Bool
doesUserOwnKeyQuery (UserID uId) (KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
          key <- all_ (_keys businessRegistryDB)
          guard_ (key_id key ==. val_ keyId)
          guard_ (val_ (UserId uId) ==. (key_user_id key))
          pure key
  return $ case r of
    [_key] -> True
    _      -> False
