{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}


module Mirza.BusinessRegistry.Handlers.Keys
  (
    getPublicKey
  , getPublicKeyInfo
  , revokePublicKey
  , addPublicKey
  , getKeyById
  ) where


import           Mirza.BusinessRegistry.Database.Schema   as Schema
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Time
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

import           Control.Monad                            (unless)
import           Data.Maybe                               (isJust)
import           Control.Monad.Error.Hoist                ((<!?>))
import           Control.Lens                             ((#))

minPubKeySize :: Bit
minPubKeySize = Bit 2048

getPublicKey :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                , Member err     '[AsKeyError, AsSqlError])
             => CT.BRKeyId
             -> AppM context err PEM_RSAPubKey
getPublicKey kid = do
  mpem <- runDb $ getPublicKeyQuery kid
  maybe (throwing _KeyNotFound kid) pure mpem

getPublicKeyQuery :: CT.BRKeyId
                  -> DB context err (Maybe PEM_RSAPubKey)
getPublicKeyQuery (CT.BRKeyId uuid) = fmap (fmap PEM_RSAPubKey) $ pg $ runSelectReturningOne $
  select $ do
    keys <- all_ (_keys businessRegistryDB)
    guard_ (primaryKey keys ==. val_ (Schema.KeyId uuid))
    pure (pem_str keys)

getPublicKeyInfo :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                    , Member err     '[AsKeyError, AsSqlError])
                 => CT.BRKeyId
                 -> AppM context err BT.KeyInfoResponse
getPublicKeyInfo kid = do
  currTime <- liftIO getCurrentTime
  mkey <- runDb $ getPublicKeyInfoQuery kid
  maybe (throwing _KeyNotFound kid)
        (pure . keyToKeyInfo currTime)
        mkey

keyToKeyInfo :: UTCTime
             -> Schema.Key
             -> KeyInfoResponse
keyToKeyInfo currTime (Schema.KeyT keyId (Schema.UserId keyUserId) pemStr creation revocation expiration ) =
  (KeyInfoResponse (CT.BRKeyId keyId) (CT.UserId keyUserId)
    (getKeyState
      (fromDbTimestamp <$> revocation)
      (fromDbTimestamp <$> expiration)
    )
    (fromDbTimestamp creation)
    (fromDbTimestamp <$> revocation)
    (fromDbTimestamp <$> expiration)
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


getPublicKeyInfoQuery :: CT.BRKeyId
                      -> DB context err (Maybe Schema.Key)
getPublicKeyInfoQuery (CT.BRKeyId uuid) = pg $ runSelectReturningOne $
  select $ do
    keys <- all_ (_keys businessRegistryDB)
    guard_ (primaryKey keys ==. val_ (Schema.KeyId uuid))
    pure keys

addPublicKey :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                , Member err     '[AsKeyError, AsSqlError])
             => BT.AuthUser
             -> PEM_RSAPubKey
             -> Maybe ExpirationTime
             -> AppM context err CT.BRKeyId
addPublicKey user pemKey@(PEM_RSAPubKey pemStr) mExp = do
  somePubKey <- liftIO $ readPublicKey (unpack pemStr) -- TODO: Catch exception from OpenSSL - any invalid PEM string causes exception
  rsaKey <- checkPubKey somePubKey pemKey              -- Input: "x"
  runDb $ addPublicKeyQuery user mExp rsaKey           -- Error: user error (error:0906D06C:PEM routines:PEM_read_bio:no start line)


checkPubKey :: (MonadError err m, AsKeyError err)
            => SomePublicKey
            -> PEM_RSAPubKey
            -> m RSAPubKey
checkPubKey spKey pemKey =
  maybe (throwing _InvalidRSAKey pemKey)
  (\pubKey ->
    -- Note: This constraint implies that we only check that the key is greater then (2^N)-8 bits since rsaSize is the
    -- number of bytes required to hold the key. See github issue #212
    -- https://github.csiro.au/Blockchain/supplyChainServer/issues/212#issuecomment-13326 for further information.
    let keySizeBits = Bit $ rsaSize pubKey * 8 in
    -- rsaSize returns size in bytes
    if keySizeBits < minPubKeySize
      then throwing _InvalidRSAKeySize (Expected minPubKeySize, Received keySizeBits)
      else pure pubKey
  )
  (toPublicKey spKey)


addPublicKeyQuery :: ( Member err     '[AsKeyError])
                  => AuthUser
                  -> Maybe ExpirationTime
                  -> RSAPubKey
                  -> DB context err CT.BRKeyId
addPublicKeyQuery (AuthUser (CT.UserId uid)) expTime rsaPubKey = do
  keyStr <- liftIO $ pack <$> writePublicKey rsaPubKey
  keyId <- newUUID
  timestamp <- generateTimestamp
  ks <- pg $ runInsertReturningList (_keys businessRegistryDB) $
        insertValues
        [ KeyT keyId (Schema.UserId uid) keyStr
            (toDbTimestamp timestamp) Nothing (toDbTimestamp <$> expTime)
        ]
  case ks of
    [rowId] -> return (CT.BRKeyId $ key_id rowId)
    _       -> throwing _PublicKeyInsertionError (map (CT.BRKeyId . key_id) ks)


revokePublicKey :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                   , Member err     '[AsKeyError, AsSqlError])
                => BT.AuthUser
                -> CT.BRKeyId
                -> AppM context err RevocationTime
revokePublicKey (AuthUser uId) keyId =
    runDb $ revokePublicKeyQuery uId keyId


keyStateQuery :: AsKeyError err
              => CT.BRKeyId
              -> DB context err KeyState
keyStateQuery kid = do
  currTime <- liftIO getCurrentTime
  keyInfoState . keyToKeyInfo currTime <$> getPublicKeyInfoQuery kid <!?> (_KeyNotFound # kid)


-- | Checks that the key is useable and throws a key error if the key is not
-- found, the user doesn't have permission to modify the key, it is expired
-- or revoked. The function can be modified in the future to add additional
-- constraints that must be checked before the key is updated in anyway
-- (effectively controling the minimum state for write access to the key).
protectKeyUpdate :: ( Member err     '[AsKeyError])
                 =>  CT.BRKeyId
                 -> CT.UserId
                 -> DB context err ()
protectKeyUpdate keyId userId = do
  userOwnsKey <- doesUserOwnKeyQuery userId keyId
  unless userOwnsKey $ throwing_ _UnauthorisedKeyAccess

  state <- keyStateQuery keyId
  case state of
    Revoked  -> throwing_ _KeyAlreadyRevoked
    Expired  -> throwing_ _KeyAlreadyExpired
    InEffect -> pure ()

revokePublicKeyQuery :: ( Member err     '[AsKeyError])
                     => CT.UserId
                     -> CT.BRKeyId
                     -> DB context err RevocationTime
revokePublicKeyQuery userId k@(CT.BRKeyId keyId) = do
  protectKeyUpdate k userId
  timestamp <- generateTimestamp
  _r <- pg $ runUpdate $ update
                (_keys businessRegistryDB)
                (\key -> [revocation_time key <-. val_ (Just $ toDbTimestamp timestamp)])
                (\key -> key_id key ==. (val_ keyId))
  return $ RevocationTime timestamp


doesUserOwnKeyQuery :: CT.UserId
                    -> CT.BRKeyId
                    -> DB context err Bool
doesUserOwnKeyQuery (CT.UserId uId) (CT.BRKeyId keyId) = do
  r <- pg $ runSelectReturningOne $ select $ do
          key <- all_ (_keys businessRegistryDB)
          guard_ (key_id key ==. val_ keyId)
          guard_ (val_ (Schema.UserId uId) ==. (key_user_id key))
          pure key
  return $ isJust r

getKeyById :: CT.BRKeyId
           -> DB context err (Maybe Key)
getKeyById (CT.BRKeyId keyId) = pg $ runSelectReturningOne $ select $ do
          key <- all_ (_keys businessRegistryDB)
          guard_ (key_id key ==. val_ keyId)
          pure key
