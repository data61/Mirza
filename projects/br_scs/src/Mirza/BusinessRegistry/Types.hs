{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Mirza.BusinessRegistry.Types (
    module Mirza.BusinessRegistry.Types
  , module CT
  ) where

import           Mirza.Common.Time                    (CreationTime,
                                                       ExpirationTime,
                                                       RevocationTime)
import           Mirza.Common.Types                   as CT

import           Data.GS1.EPC                         as EPC

import           Data.Pool                            as Pool

import           Database.Beam
import           Database.Beam.Backend.SQL
import qualified Database.Beam.Migrate                as BMigrate
import qualified Database.Beam.Postgres               as BPostgres
import           Database.Beam.Postgres.Syntax        (PgDataTypeSyntax)
import           Database.PostgreSQL.Simple           (Connection, SqlError)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.ToField   (ToField, toField)

import           Crypto.JOSE                          (JWK)
import           Crypto.JWT                           (Audience, ClaimsSet,
                                                       claimSub, string)

import qualified Servant.Auth.Server                  as SAS

import           Katip                                as K

import           Network.URI                          (URI)

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types

import           Data.Swagger
import           Data.Text                            (Text)
import           Data.Time                            (LocalTime)

import           Control.Lens
import           Data.Proxy                           (Proxy (..))

import           GHC.Generics                         (Generic)
import           GHC.Stack                            (CallStack)

-- *****************************************************************************
-- Context Types
-- *****************************************************************************

type BRContextMinimal  = BRContextGeneric () ()
type BRContextComplete = BRContextGeneric Audience JWK

brContextMinimal :: EnvType -> Pool Connection -> K.LogEnv -> K.LogContexts -> K.Namespace -> BRContextMinimal
brContextMinimal a b c d e = BRContextGeneric a b c d e () ()

brContextComplete :: BRContextMinimal -> Audience -> JWK-> BRContextComplete
brContextComplete (BRContextGeneric a b c d e () ()) f g = BRContextGeneric a b c d e f g

data BRContextGeneric audienceType publicKeyType = BRContextGeneric
  { _brEnvType          :: EnvType
  , _brDbConnPool       :: Pool Connection
  , _brKatipLogEnv      :: K.LogEnv
  , _brKatipLogContexts :: K.LogContexts
  , _brKatipNamespace   :: K.Namespace
  , _brAuthAudience     :: audienceType
  , _brAuthPublicKey    :: publicKeyType
  }
$(makeLenses ''BRContextGeneric)

instance HasEnvType (BRContextGeneric a b) where
  envType = brEnvType
instance HasConnPool (BRContextGeneric a b) where
  connPool = brDbConnPool
instance HasKatipLogEnv (BRContextGeneric a b) where
  katipLogEnv = brKatipLogEnv
instance HasKatipContext (BRContextGeneric a b) where
  katipContexts = brKatipLogContexts
  katipNamespace = brKatipNamespace
instance HasAuthAudience (BRContextGeneric Audience a) where
  authAudience = brAuthAudience
instance HasAuthPublicKey (BRContextGeneric a JWK) where
  authPublicKey = brAuthPublicKey


-- Lenses that are only required by the Business Registry.

class HasAuthAudience a where
  authAudience :: Lens' a (Audience)
class HasAuthPublicKey a where
  authPublicKey :: Lens' a (JWK)


-- | Convenience class for contexts which can be used for verifying JWKs.
-- @
--   foo :: Member context '[HasLogging] => Foo -> DB context err Bar
-- @
class (HasAuthAudience context, HasAuthPublicKey context)
  => HasJWKVerificationInformation context where
instance (HasAuthAudience context, HasAuthPublicKey context)
  => HasJWKVerificationInformation context


-- *****************************************************************************
-- Service Response Types
-- *****************************************************************************

-- Note: The definitions in this section are reverse order defined(more specific
--       to more general rather then overview to more detail) because the
--       template haskell used defines that they must be ordered in this way, so
--       they are grouped by theme and commented as such, but the order within
--       a section appears logically bottom to top, rather then the normal top
--       to bottom.



-- | Note that BusinessRegistry.NewUser is expected to become different in the
-- future, and hence this duplication
data NewUser = NewUser
  { newUserOAuthSub     :: Text
  } deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewUser)
instance ToSchema NewUser

-- Auth User Types:
newtype VerifiedTokenClaims = VerifiedTokenClaims
  { verifiedTokenClaimsSub :: Text
  } deriving (Show)
instance SAS.ToJWT VerifiedTokenClaims where
  encodeJWT = error "Not implemented" -- TODO: Implement this properly

instance SAS.FromJWT VerifiedTokenClaims where
  decodeJWT :: ClaimsSet -> Either Text VerifiedTokenClaims
  decodeJWT claims = maybeToEither "No sub present in token" maybeVerifiedTokenClaims where
    maybeStringOrURISub = view claimSub claims
    maybeTextSub = (view string) <$> maybeStringOrURISub
    maybeVerifiedTokenClaims = VerifiedTokenClaims <$> maybeTextSub
    maybeToEither leftText = maybe (Left leftText) Right


newtype AuthUser = AuthUser { authUserId :: UserId }
  deriving (Show, Eq, Read, Generic)
instance ToSchema AuthUser
instance ToParamSchema AuthUser


data NewBusiness = NewBusiness
  { newBusinessGS1CompanyPrefix :: GS1CompanyPrefix
  , newBusinessName             :: Text
  , newBusinessUrl              :: Network.URI.URI
  } deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewBusiness)
instance ToSchema NewBusiness

-- Business Response Types:
data BusinessResponse = BusinessResponse
  { businessGS1CompanyPrefix :: EPC.GS1CompanyPrefix
  , businessName             :: Text
  , businessUrl              :: Network.URI.URI
  }
  deriving (Show, Eq, Generic)
instance ToSchema BusinessResponse
instance ToJSON BusinessResponse
instance FromJSON BusinessResponse


instance ToSchema Network.URI.URI where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
    <&> name ?~ "URI"
    <&> schema . description ?~ "An RFC 3986 compliant URI."


data NewLocation = NewLocation
  { newLocGLN     :: LocationEPC
  , newLocCoords  :: Maybe (Latitude, Longitude)
  , newLocAddress :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance ToSchema NewLocation
instance ToJSON NewLocation
instance FromJSON NewLocation

newtype Latitude  = Latitude  { getLatitude  :: Double }
        deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic)
newtype Longitude = Longitude { getLongitude :: Double }
        deriving (Show, Eq, Ord, FromJSON, ToJSON, Generic)

instance ToSchema Latitude
instance ToSchema Longitude

instance HasSqlValueSyntax be Double
      => HasSqlValueSyntax be Latitude where
  sqlValueSyntax = sqlValueSyntax . getLatitude
instance (BMigrate.IsSql92ColumnSchemaSyntax be)
      => BMigrate.HasDefaultSqlDataTypeConstraints be Latitude

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool
         , IsSql92ExpressionSyntax be)
      => HasSqlEqualityCheck be Latitude
instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool
         , IsSql92ExpressionSyntax be)
      => HasSqlQuantifiedEqualityCheck be Latitude

instance FromBackendRow BPostgres.Postgres Latitude where
  fromBackendRow = Latitude <$> fromBackendRow

instance FromField Latitude where
  fromField fld mbs = Latitude <$> fromField fld mbs

instance ToField Latitude where
  toField = toField . getLatitude

latitudeType :: BMigrate.DataType PgDataTypeSyntax Latitude
latitudeType = BMigrate.DataType doubleType


instance HasSqlValueSyntax be Double
      => HasSqlValueSyntax be Longitude where
  sqlValueSyntax = sqlValueSyntax . getLongitude
instance BMigrate.IsSql92ColumnSchemaSyntax be
      => BMigrate.HasDefaultSqlDataTypeConstraints be Longitude

instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool
         , IsSql92ExpressionSyntax be)
      => HasSqlEqualityCheck be Longitude
instance ( HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool
         , IsSql92ExpressionSyntax be)
      => HasSqlQuantifiedEqualityCheck be Longitude

instance FromBackendRow BPostgres.Postgres Longitude where
  fromBackendRow = Longitude <$> fromBackendRow

instance FromField Longitude where
  fromField fld mbs = Longitude <$> fromField fld mbs

instance ToField Longitude where
  toField = toField . getLongitude

longitudeType :: BMigrate.DataType PgDataTypeSyntax Longitude
longitudeType = BMigrate.DataType doubleType


data LocationResponse = LocationResponse
  { locationId    :: PrimaryKeyType
  , locationGLN   :: EPC.LocationEPC
  , locationBiz   :: GS1CompanyPrefix
  , geoLocId      :: PrimaryKeyType
  , geoLocCoord   :: Maybe (Latitude, Longitude)
  , geoLocAddress :: Maybe Text
  } deriving (Show, Generic, Eq)
instance ToSchema LocationResponse
instance ToJSON LocationResponse
instance FromJSON LocationResponse


data BusinessAndLocationResponse = BusinessAndLocationResponse
  { businessResponse :: BusinessResponse
  , locationResponse :: LocationResponse
  } deriving (Show, Generic, Eq)
instance ToSchema BusinessAndLocationResponse
instance ToJSON BusinessAndLocationResponse
instance FromJSON BusinessAndLocationResponse


data KeyState
  = InEffect -- Can be used
  | Revoked -- Key passed the revocation time
  | Expired -- Key passed the expiration time
  deriving (Show, Eq, Read, Generic)
$(deriveJSON defaultOptions ''KeyState)
instance ToSchema KeyState
instance ToParamSchema KeyState


-- Health Types:
successHealthResponseText :: Text
successHealthResponseText = "Status OK"

data HealthResponse = HealthResponse
  deriving (Show, Eq, Read, Generic)
instance ToSchema HealthResponse
instance ToJSON HealthResponse where
  toJSON _ = toJSON successHealthResponseText
instance FromJSON HealthResponse where
  parseJSON (String value)
    | value == successHealthResponseText = pure HealthResponse
    | otherwise                          = fail "Invalid health response string."
  parseJSON value                        = typeMismatch "HealthResponse" value



-- *****************************************************************************
-- Signing and Hashing Types
-- *****************************************************************************


data KeyInfoResponse = KeyInfoResponse
  { keyInfoId             :: CT.BRKeyId
  , keyInfoUserId         :: UserId  -- TODO: There should be a forien key for Business in here....not sure that user is relevant...
  , keyInfoState          :: KeyState
  , keyInfoCreationTime   :: CreationTime
  , keyInfoRevocation     :: Maybe (RevocationTime, UserId)
  , keyInfoExpirationTime :: Maybe ExpirationTime
  , keyInfoJWK            :: JWK
  }
  deriving (Generic, Show, Eq)
$(deriveJSON defaultOptions ''KeyInfoResponse)
instance ToSchema KeyInfoResponse


-- *****************************************************************************
-- Error Types
-- *****************************************************************************

data BRError
  = DBErrorBRE SqlError
  | UnmatchedUniqueViolationBRE SqlError
  -- | The user tried to add a business with the a GS1CompanyPrefix that already exsits.
  | GS1CompanyPrefixExistsBRE
  | BusinessDoesNotExistBRE
  | UserAuthFailureBRE (SAS.AuthResult ())
  -- | When adding a user fails for an unknown reason.
  | UserCreationErrorBRE String CallStack
  | BRKeyErrorBRE BRKeyError
  | LocationNotKnownBRE
  | LocationExistsBRE
  | UnknownUserBRE
  | OperationNotPermittedBRE GS1CompanyPrefix UserId
  -- | An error that isn't specifically excluded by the types, but that the
  -- developers don't think is possible to hit, or know of a situation which
  -- could cause this case to be exercised.
  | UnexpectedErrorBRE CallStack
  deriving (Show, Generic)

data BRKeyError
  = InvalidRSAKeyBRKE JWK
  | InvalidRSAKeySizeBRKE Expected Received
  | KeyIsPrivateKeyBRKE
  | PublicKeyInsertionErrorBRKE [CT.BRKeyId]
  | KeyNotFoundBRKE CT.BRKeyId
  | UnauthorisedKeyAccessBRKE
  | KeyAlreadyRevokedBRKE
  | KeyAlreadyExpiredBRKE
  -- | If it is detected that the key has a revocation time and no revoking
  -- user or the key has a revoking user but now revoking time. Hopefully in
  -- practice it is not possible to produce this error since it probably
  -- indicates a bug in our code. It is only possible to generate this error
  -- because we don't store the revoking data in the database as a
  -- Maybe (Time, User) because this is technically complex. If we encounter
  -- this error it might be a good time to re-evaulate whether it is better to
  -- fix the storage datatype so its not possible to generate this error in the
  -- first place.
  | InvalidRevocationBRKE (Maybe LocalTime) (Maybe PrimaryKeyType) CallStack
  | AddedExpiredKeyBRKE
  deriving (Show)

newtype Bit  = Bit  {getBit :: Int} deriving (Show, Eq, Read, Ord)
newtype Expected = Expected {getExpected :: Bit} deriving (Show, Eq, Read, Ord)
newtype Received = Received {getReceived :: Bit} deriving (Show, Eq, Read, Ord)


-- Lens definitions for Error Types.
$(makeClassyPrisms ''BRError)
$(makeClassyPrisms ''BRKeyError)

instance AsSqlError BRError where _SqlError = _DBErrorBRE
instance AsBRKeyError BRError where _BRKeyError = _BRKeyErrorBRE
