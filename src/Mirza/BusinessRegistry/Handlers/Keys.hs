{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Mirza.BusinessRegistry.Handlers.Keys
  (
    getPublicKey
  , getPublicKeyInfo
  , revokePublicKey
  , addPublicKey
  , getKeyById
  , getKeyState
  ) where


import           Mirza.BusinessRegistry.Database.Schema   as Schema
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Time
import           Mirza.Common.Types                       as CT
import           Mirza.Common.Utils

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres                   (PgJSON (..))

import           Data.Time.Clock                          (UTCTime,
                                                           getCurrentTime)
import           Data.Time.LocalTime

import           Control.Lens                             (( # ), (^.))
import           Control.Monad                            (unless, when)
import           Control.Monad.Error.Hoist                ((<!?>))
import           Data.Foldable                            (for_)
import           Data.Maybe                               (isJust, isNothing)
import           GHC.Stack                                (HasCallStack,
                                                           callStack)

import           Crypto.JOSE.JWA.JWK                      (KeyMaterial (RSAKeyMaterial),
                                                           RSAKeyParameters (..),
                                                           rsaPublicKey)
import           Crypto.JOSE.JWK                          (JWK, jwkMaterial)
import           Crypto.PubKey.RSA.Types                  (public_size)


minPubKeySize :: Bit
minPubKeySize = Bit 2048

getPublicKey :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                , Member err     '[AsBRKeyError, AsSqlError])
             => CT.BRKeyId
             -> AppM context err JWK
getPublicKey kid = fromPgJSON <$>
  runDb (getPublicKeyQuery kid <!?> (_KeyNotFoundBRKE # kid))

getPublicKeyQuery :: CT.BRKeyId
                  -> DB context err (Maybe (PgJSON JWK))
getPublicKeyQuery (CT.BRKeyId uuid) = pg $ runSelectReturningOne $
  select $ do
    keys <- all_ (_keys businessRegistryDB)
    guard_ (primaryKey keys ==. val_ (Schema.KeyId uuid))
    pure (key_jwk keys)

getPublicKeyInfo :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                    , Member err     '[AsBRKeyError, AsSqlError])
                 => CT.BRKeyId
                 -> AppM context err BT.KeyInfoResponse
getPublicKeyInfo kid = do
  currTime <- liftIO getCurrentTime
  key <- runDb $ getPublicKeyInfoQuery kid <!?> (_KeyNotFoundBRKE # kid)
  keyToKeyInfo currTime key


keyToKeyInfo :: (MonadError err m, AsBRKeyError err)
             => UTCTime
             -> Schema.Key
             -> m KeyInfoResponse
keyToKeyInfo _ (Schema.KeyT _ _ _ _ _ _ _ Nothing) = error "keyToKeyInfo: Received Nothing last modified time!"
keyToKeyInfo currTime (Schema.KeyT keyId (Schema.UserId keyUserId) (PgJSON jwk)
                                   creation revocationTime revocationUser
                                   expiration _)
  = do
  revocation <- composeRevocation revocationTime revocationUser
  pure $ KeyInfoResponse (CT.BRKeyId keyId) (CT.UserId keyUserId)
    (getKeyState currTime
      (fromDbTimestamp <$> revocationTime)
      (fromDbTimestamp <$> expiration)
    )
    (fromDbTimestamp creation)
    revocation
    (fromDbTimestamp <$> expiration)
    jwk
  where
    -- | This function checks that the Maybe constructor for both the time and
    -- the user matches (i.e. both Just, or both Nothing) and throws an error if
    -- this is not the case. Logically they should only ever be the same, since
    -- both the user and time should be recorded when the key is revoked, but
    -- since we store in the database as two separate fields (because of
    -- complexity storing natively as a (Maybe (time, user)), see database
    -- comment for more info) we need to verify when we combine them here.
    composeRevocation :: (HasCallStack, MonadError e m, AsBRKeyError e, ModelTimestamp a)
                      => Maybe LocalTime
                      -> PrimaryKey UserT (Nullable Identity)
                      -> m (Maybe (a, CT.UserId))
    composeRevocation time@Nothing  (Schema.UserId (Just user)) = throwing _InvalidRevocationBRKE (time, Just user, callStack)
    composeRevocation time@(Just _) (Schema.UserId Nothing)     = throwing _InvalidRevocationBRKE (time, Nothing, callStack)
    composeRevocation time          (Schema.UserId user)        = pure ((,) <$> (fromDbTimestamp <$> time) <*> (CT.UserId  <$> user))


-- TODO: After migrating entirely to X509, there should always be an expiration
-- time (no Maybe wrapping ExpirationTime).
getKeyState :: UTCTime
            -> Maybe RevocationTime
            -> Maybe ExpirationTime
            -> KeyState
-- order of precedence - Revoked > Expired
getKeyState currTime (Just (RevocationTime rTime)) (Just (ExpirationTime eTime))
  | mkUtcMicros currTime >= rTime = Revoked
  | mkUtcMicros currTime >= eTime = Expired
  | otherwise        = InEffect
getKeyState currTime Nothing (Just (ExpirationTime eTime))
  | mkUtcMicros currTime >= eTime = Expired
  | otherwise        = InEffect
getKeyState currTime (Just (RevocationTime rTime)) Nothing
  | mkUtcMicros currTime >= rTime = Revoked
  | otherwise        = InEffect
getKeyState _ Nothing Nothing = InEffect


getPublicKeyInfoQuery :: CT.BRKeyId
                      -> DB context err (Maybe Schema.Key)
getPublicKeyInfoQuery (CT.BRKeyId uuid) = pg $ runSelectReturningOne $
  select $ do
    keys <- all_ (_keys businessRegistryDB)
    guard_ (primaryKey keys ==. val_ (Schema.KeyId uuid))
    pure keys

addPublicKey :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                , Member err     '[AsBRKeyError, AsSqlError])
             => BT.AuthUser
             -> JWK
             -> Maybe ExpirationTime
             -> AppM context err CT.BRKeyId
addPublicKey user jwk mExp = do
  checkJWKPubKey jwk
  runDb $ addPublicKeyQuery user mExp jwk

-- | Checks the validity of a JWK to ensure that it is
-- a) an RSA public key
-- b) at least `minPubKeySize`-7 bits (since it's technically possible to have an RSA
-- public key which is n/8 bytes but not n bits)
-- c) Key does not also contain a private key
checkJWKPubKey :: ( Member err '[AsBRKeyError], MonadError err m)
               => JWK
               -> m ()
checkJWKPubKey jwk =
  case jwk ^. jwkMaterial of
    RSAKeyMaterial params@(RSAKeyParameters _ _ Nothing) ->
      let keysize = Bit (8 * public_size{-bytes-} (rsaPublicKey params))
      in when (keysize < minPubKeySize) $
          throwing _InvalidRSAKeySizeBRKE (Expected minPubKeySize, Received keysize)
    RSAKeyMaterial (RSAKeyParameters _ _ (Just _)) -> throwing_ _KeyIsPrivateKeyBRKE
    _ -> throwing _InvalidRSAKeyBRKE jwk


addPublicKeyQuery :: ( Member err     '[AsBRKeyError])
                  => AuthUser
                  -> Maybe ExpirationTime
                  -> JWK
                  -> DB context err CT.BRKeyId
addPublicKeyQuery (AuthUser (CT.UserId uid)) expTime jwk = do
  now <- liftIO getCurrentTime
  for_ expTime $ \time -> when ((getExpirationTime time) <= mkUtcMicros now) (throwing_ _AddedExpiredKeyBRKE)
  keyId <- newUUID
  timestamp <- generateTimestamp
  ks <- pg $ runInsertReturningList (_keys businessRegistryDB) $
        insertValues
        [ KeyT keyId (Schema.UserId uid) (PgJSON jwk)
            (toDbTimestamp timestamp) Nothing (Schema.UserId Nothing) (toDbTimestamp <$> expTime)
            Nothing
        ]
  case ks of
    [rowId] -> pure (CT.BRKeyId $ key_id rowId)
    _       -> throwing _PublicKeyInsertionErrorBRKE (map (CT.BRKeyId . key_id) ks)


revokePublicKey :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                   , Member err     '[AsBRKeyError, AsSqlError])
                => BT.AuthUser
                -> CT.BRKeyId
                -> AppM context err RevocationTime
revokePublicKey (AuthUser uId) keyId =
    runDb $ revokePublicKeyQuery uId keyId


keyStateQuery :: AsBRKeyError err
              => CT.BRKeyId
              -> DB context err KeyState
keyStateQuery kid = do
  currTime <- liftIO getCurrentTime
  fmap keyInfoState . keyToKeyInfo currTime =<< getPublicKeyInfoQuery kid <!?> (_KeyNotFoundBRKE # kid)


-- | Checks that the key is useable and throws a key error if the key is not
-- found, the user doesn't have permission to modify the key, it is expired
-- or revoked. The function can be modified in the future to add additional
-- constraints that must be checked before the key is updated in anyway
-- (effectively controling the minimum state for write access to the key).
protectKeyUpdate :: ( Member err     '[AsBRKeyError])
                 => CT.BRKeyId
                 -> CT.UserId
                 -> DB context err ()
protectKeyUpdate keyId userId = do
  -- Although this check is implictly performed in keyStateQuery we explicitly perform it here so that the code reads
  -- logically and progressively builds constraints in order and so that the correct error message is shown when the
  -- keyId is not found, rather then failing because the user id check in doesUserOwnKeyQuery fails.
  key <- getKeyById keyId
  when (isNothing key) $ throwing _KeyNotFoundBRKE keyId

  userOwnsKey <- doesUserOwnKeyQuery userId keyId
  unless userOwnsKey $ throwing_ _UnauthorisedKeyAccessBRKE

  state <- keyStateQuery keyId
  case state of
    Revoked  -> throwing_ _KeyAlreadyRevokedBRKE
    Expired  -> throwing_ _KeyAlreadyExpiredBRKE
    InEffect -> pure ()

revokePublicKeyQuery :: ( Member err     '[AsBRKeyError])
                     => CT.UserId
                     -> CT.BRKeyId
                     -> DB context err RevocationTime
revokePublicKeyQuery userId k@(CT.BRKeyId keyId) = do
  protectKeyUpdate k userId
  timestamp <- generateTimestamp
  _r <- pg $ runUpdate $ update
                (_keys businessRegistryDB)
                (\key -> [ revocation_time key  <-. val_ (Just $ toDbTimestamp timestamp)
                         , revoking_user_id key <-. val_ (Schema.UserId $ Just $ getUserId userId)])
                (\key -> key_id key ==. (val_ keyId))
  pure $ RevocationTime $ mkUtcMicros timestamp


doesUserOwnKeyQuery :: CT.UserId
                    -> CT.BRKeyId
                    -> DB context err Bool
doesUserOwnKeyQuery (CT.UserId uId) (CT.BRKeyId keyId) = do
  r <- pg $ runSelectReturningOne $ select $ do
          key <- all_ (_keys businessRegistryDB)
          guard_ (key_id key ==. val_ keyId)
          guard_ (val_ (Schema.UserId uId) ==. (key_user_id key))
          pure key
  pure $ isJust r

getKeyById :: CT.BRKeyId
           -> DB context err (Maybe Key)
getKeyById (CT.BRKeyId keyId) = pg $ runSelectReturningOne $ select $ do
          key <- all_ (_keys businessRegistryDB)
          guard_ (key_id key ==. val_ keyId)
          pure key
