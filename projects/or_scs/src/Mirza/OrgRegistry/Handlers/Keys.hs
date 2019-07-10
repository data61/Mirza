{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Mirza.OrgRegistry.Handlers.Keys
  (
    getPublicKey
  , getPublicKeyInfo
  , revokePublicKey
  , addPublicKey
  , getKeyById
  , getKeyState
  ) where

import           Mirza.Common.Time
import           Mirza.Common.Types                       as CT
import           Mirza.Common.Utils
import           Mirza.OrgRegistry.Auth
import           Mirza.OrgRegistry.Database.Schema        as Schema
import           Mirza.OrgRegistry.Types                  as ORT

import           Data.GS1.EPC                             as EPC (GS1CompanyPrefix)

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Postgres                   (PgJSON (..))

import           Data.Time.Clock                          (UTCTime,
                                                           getCurrentTime)
import           Data.Time.LocalTime

import           Control.Lens                             (( # ), (^.))
import           Control.Monad                            (when)
import           Control.Monad.Error.Hoist                ((<!?>))
import           Data.Foldable                            (for_)
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
                , Member err     '[AsORKeyError, AsSqlError])
             => CT.ORKeyId
             -> AppM context err JWK
getPublicKey kid = fromPgJSON <$>
  runDb (getPublicKeyQuery kid <!?> (_KeyNotFoundORKE # kid))

getPublicKeyQuery :: CT.ORKeyId
                  -> DB context err (Maybe (PgJSON JWK))
getPublicKeyQuery (CT.ORKeyId uuid) = pg $ runSelectReturningOne $
  select $ do
    keys <- all_ (_keys orgRegistryDB)
    guard_ (primaryKey keys ==. val_ (Schema.KeyPrimaryKey uuid))
    pure (key_jwk keys)

getPublicKeyInfo :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                    , Member err     '[AsORKeyError, AsSqlError])
                 => CT.ORKeyId
                 -> AppM context err ORT.KeyInfoResponse
getPublicKeyInfo kid = do
  currTime <- liftIO getCurrentTime
  key <- runDb $ getPublicKeyInfoQuery kid <!?> (_KeyNotFoundORKE # kid)
  keyToKeyInfo currTime key


keyToKeyInfo :: (MonadError err m, AsORKeyError err)
             => UTCTime
             -> Schema.Key
             -> m KeyInfoResponse
keyToKeyInfo _ (Schema.KeyT _ _ _ _ _ _ _ Nothing) = error "keyToKeyInfo: Received Nothing last modified time!"
keyToKeyInfo currTime (Schema.KeyT keyId (Schema.OrgPrimaryKey org) (PgJSON jwk)
                                   creation revocationTime revocationUser
                                   expiration _)
  = do
  revocation <- composeRevocation revocationTime revocationUser
  pure $ KeyInfoResponse (CT.ORKeyId keyId) org
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
    composeRevocation :: (HasCallStack, MonadError e m, AsORKeyError e, ModelTimestamp a)
                      => Maybe LocalTime
                      -> PrimaryKey UserT (Nullable Identity)
                      -> m (Maybe (a, OAuthSub))
    composeRevocation time@Nothing  (Schema.UserPrimaryKey (Just user)) = throwing _InvalidRevocationORKE (time, Just user, callStack)
    composeRevocation time@(Just _) (Schema.UserPrimaryKey Nothing)     = throwing _InvalidRevocationORKE (time, Nothing, callStack)
    composeRevocation time          (Schema.UserPrimaryKey oAuthSub)    = pure $ ((,) <$> (fromDbTimestamp <$> time) <*> oAuthSub)


-- TODO: After migrating entirely to X509, there should always be an expiration
-- time (no Maybe wrapping ExpirationTime).
getKeyState :: UTCTime
            -> Maybe RevocationTime
            -> Maybe ExpirationTime
            -> KeyState
-- order of precedence - Revoked > Expired
getKeyState currTime (Just (RevocationTime rTime)) (Just (ExpirationTime eTime))
  | currTime >= rTime = Revoked
  | currTime >= eTime = Expired
  | otherwise        = InEffect
getKeyState currTime Nothing (Just (ExpirationTime eTime))
  | currTime >= eTime = Expired
  | otherwise        = InEffect
getKeyState currTime (Just (RevocationTime rTime)) Nothing
  | currTime >= rTime = Revoked
  | otherwise        = InEffect
getKeyState _ Nothing Nothing = InEffect


getPublicKeyInfoQuery :: CT.ORKeyId
                      -> DB context err (Maybe Schema.Key)
getPublicKeyInfoQuery (CT.ORKeyId uuid) = pg $ runSelectReturningOne $
  select $ do
    keys <- all_ (_keys orgRegistryDB)
    guard_ (primaryKey keys ==. val_ (Schema.KeyPrimaryKey uuid))
    pure keys

addPublicKey :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                , Member err     '[AsORKeyError, AsORError, AsSqlError])
             => ORT.AuthUser
             -> GS1CompanyPrefix
             -> JWK
             -> Maybe ExpirationTime
             -> AppM context err CT.ORKeyId
addPublicKey user org jwk mExp = do
  checkJWKPubKey jwk
  runDb $ addPublicKeyQuery user org mExp jwk

-- | Checks the validity of a JWK to ensure that it is
-- a) an RSA public key
-- b) at least `minPubKeySize`-7 bits (since it's technically possible to have an RSA
-- public key which is n/8 bytes but not n bits)
-- c) Key does not also contain a private key
checkJWKPubKey :: ( Member err '[AsORKeyError], MonadError err m)
               => JWK
               -> m ()
checkJWKPubKey jwk =
  case jwk ^. jwkMaterial of
    RSAKeyMaterial params@(RSAKeyParameters _ _ Nothing) ->
      let keysize = Bit (8 * public_size{-bytes-} (rsaPublicKey params))
      in when (keysize < minPubKeySize) $
          throwing _InvalidRSAKeySizeORKE (Expected minPubKeySize, Received keysize)
    RSAKeyMaterial (RSAKeyParameters _ _ (Just _)) -> throwing_ _KeyIsPrivateKeyORKE
    _ -> throwing _InvalidRSAKeyORKE jwk


addPublicKeyQuery :: ( Member err     '[AsORKeyError, AsORError])
                  => AuthUser
                  -> GS1CompanyPrefix
                  -> Maybe ExpirationTime
                  -> JWK
                  -> DB context err CT.ORKeyId
addPublicKeyQuery authUser org expTime jwk = do
  _ <- userOrganisationAuthorisationQuery authUser org

  now <- liftIO getCurrentTime
  for_ expTime $ \time -> when ((getExpirationTime time) <= now) (throwing_ _AddedExpiredKeyORKE)
  keyId <- newUUID
  timestamp <- generateTimestamp
  ks <- pg $ runInsertReturningList $ insert (_keys orgRegistryDB) $
        insertValues
        [ KeyT keyId (Schema.OrgPrimaryKey org) (PgJSON jwk)
            (toDbTimestamp timestamp) Nothing (Schema.UserPrimaryKey Nothing) (toDbTimestamp <$> expTime)
            Nothing
        ]
  case ks of
    [rowId] -> pure (CT.ORKeyId $ key_id rowId)
    _       -> throwing _PublicKeyInsertionErrorORKE (map (CT.ORKeyId . key_id) ks)


revokePublicKey :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                   , Member err     '[AsORKeyError, AsORError, AsSqlError])
                => ORT.AuthUser
                -> CT.ORKeyId
                -> AppM context err RevocationTime
revokePublicKey authUser keyId =
    runDb $ revokePublicKeyQuery authUser keyId


keyStateQuery :: AsORKeyError err
              => CT.ORKeyId
              -> DB context err KeyState
keyStateQuery kid = do
  currTime <- liftIO getCurrentTime
  fmap keyInfoState . keyToKeyInfo currTime =<< getPublicKeyInfoQuery kid <!?> (_KeyNotFoundORKE # kid)


-- | Checks that the key is useable and throws a key error if the key is not
-- found, the user doesn't have permission to modify the key, it is expired
-- or revoked. The function can be modified in the future to add additional
-- constraints that must be checked before the key is updated in anyway
-- (effectively controling the minimum state for write access to the key).
protectKeyUpdate :: ( Member err     '[AsORKeyError, AsORError])
                 => AuthUser
                 -> CT.ORKeyId
                 -> DB context err ()
protectKeyUpdate authUser keyId = do
  -- Although this check is implictly performed in keyStateQuery we explicitly perform it here so that the code reads
  -- logically and progressively builds constraints in order and so that the correct error message is shown when the
  -- keyId is not found, rather then failing because the user id check in doesUserOwnKeyQuery fails.
  maybeKey <- getKeyById keyId

  key <- maybe (throwing _KeyNotFoundORKE keyId) pure maybeKey
  _ <- userOrganisationAuthorisationQuery authUser $ orgPrimaryKeyToGS1CompanyPrefix $ key_org key

  state <- keyStateQuery keyId
  case state of
    Revoked  -> throwing_ _KeyAlreadyRevokedORKE
    Expired  -> throwing_ _KeyAlreadyExpiredORKE
    InEffect -> pure ()



revokePublicKeyQuery :: ( Member err     '[AsORKeyError, AsORError])
                     => AuthUser
                     -> CT.ORKeyId
                     -> DB context err RevocationTime
revokePublicKeyQuery authUser k@(CT.ORKeyId keyId) = do
  protectKeyUpdate authUser k
  timestamp <- generateTimestamp
  _r <- pg $ runUpdate $ update
                (_keys orgRegistryDB)
                (\key -> mconcat
                          [ revocation_time key  <-. val_ (Just $ toDbTimestamp timestamp)
                          , revoking_user_id key <-. val_ (Schema.UserPrimaryKey $ Just $ authUserId authUser)])
                (\key -> key_id key ==. (val_ keyId))
  pure $ RevocationTime timestamp


getKeyById :: CT.ORKeyId
           -> DB context err (Maybe Key)
getKeyById (CT.ORKeyId keyId) = pg $ runSelectReturningOne $ select $ do
          key <- all_ (_keys orgRegistryDB)
          guard_ (key_id key ==. val_ keyId)
          pure key
