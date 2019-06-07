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

module Mirza.OrgRegistry.Types (
    module Mirza.OrgRegistry.Types
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
import           Database.Beam.Postgres               (Postgres)
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

type ORContextMinimal  = ORContextGeneric () ()
type ORContextComplete = ORContextGeneric Audience JWK

orContextMinimal :: EnvType -> Pool Connection -> K.LogEnv -> K.LogContexts -> K.Namespace -> ORContextMinimal
orContextMinimal a b c d e = ORContextGeneric a b c d e () ()

orContextComplete :: ORContextMinimal -> Audience -> JWK-> ORContextComplete
orContextComplete (ORContextGeneric a b c d e () ()) f g = ORContextGeneric a b c d e f g

data ORContextGeneric audienceType publicKeyType = ORContextGeneric
  { _orEnvType          :: EnvType
  , _orDbConnPool       :: Pool Connection
  , _orKatipLogEnv      :: K.LogEnv
  , _orKatipLogContexts :: K.LogContexts
  , _orKatipNamespace   :: K.Namespace
  , _orAuthAudience     :: audienceType
  , _orAuthPublicKey    :: publicKeyType
  }
$(makeLenses ''ORContextGeneric)

instance HasEnvType (ORContextGeneric a b) where
  envType = orEnvType
instance HasConnPool (ORContextGeneric a b) where
  connPool = orDbConnPool
instance HasKatipLogEnv (ORContextGeneric a b) where
  katipLogEnv = orKatipLogEnv
instance HasKatipContext (ORContextGeneric a b) where
  katipContexts = orKatipLogContexts
  katipNamespace = orKatipNamespace
instance HasAuthAudience (ORContextGeneric Audience a) where
  authAudience = orAuthAudience
instance HasAuthPublicKey (ORContextGeneric a JWK) where
  authPublicKey = orAuthPublicKey


-- Lenses that are only required by the Org Registry.

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



-- | Note that OrgRegistry.NewUser is expected to become different in the
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


data PartialNewOrg = PartialNewOrg
  { partialNewOrgName :: Text
  , partialNewOrgUrl  :: Network.URI.URI
  } deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''PartialNewOrg)
instance ToSchema PartialNewOrg

data NewOrg = NewOrg
  { newOrgGS1CompanyPrefix :: GS1CompanyPrefix
  , newOrgName             :: Text
  , newOrgUrl              :: Network.URI.URI
  } deriving (Generic, Eq, Show)

-- Org Response Types:
data OrgResponse = OrgResponse
  { orgGS1CompanyPrefix :: EPC.GS1CompanyPrefix
  , orgName             :: Text
  , orgUrl              :: Network.URI.URI
  }
  deriving (Show, Eq, Generic)
instance ToSchema OrgResponse
instance ToJSON OrgResponse
instance FromJSON OrgResponse


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

instance BeamSqlBackend be => HasSqlEqualityCheck be Latitude
instance BeamSqlBackend be => HasSqlQuantifiedEqualityCheck be Latitude

instance FromBackendRow BPostgres.Postgres Latitude where
  fromBackendRow = Latitude <$> fromBackendRow

instance FromField Latitude where
  fromField fld mbs = Latitude <$> fromField fld mbs

instance ToField Latitude where
  toField = toField . getLatitude

latitudeType :: DataType Postgres Latitude
latitudeType = DataType doubleType

instance HasSqlValueSyntax be Double
      => HasSqlValueSyntax be Longitude where
  sqlValueSyntax = sqlValueSyntax . getLongitude

instance BeamSqlBackend be => HasSqlEqualityCheck be Longitude
instance BeamSqlBackend be => HasSqlQuantifiedEqualityCheck be Longitude

instance FromBackendRow BPostgres.Postgres Longitude where
  fromBackendRow = Longitude <$> fromBackendRow

instance FromField Longitude where
  fromField fld mbs = Longitude <$> fromField fld mbs

instance ToField Longitude where
  toField = toField . getLongitude

longitudeType :: DataType Postgres Longitude
longitudeType = DataType doubleType


data LocationResponse = LocationResponse
  { locationId    :: PrimaryKeyType
  , locationGLN   :: EPC.LocationEPC
  , locationOrg   :: GS1CompanyPrefix
  , geoLocId      :: PrimaryKeyType
  , geoLocCoord   :: Maybe (Latitude, Longitude)
  , geoLocAddress :: Maybe Text
  } deriving (Show, Generic, Eq)
instance ToSchema LocationResponse
instance ToJSON LocationResponse
instance FromJSON LocationResponse


data OrgAndLocationResponse = OrgAndLocationResponse
  { orgResponse      :: OrgResponse
  , locationResponse :: LocationResponse
  } deriving (Show, Generic, Eq)
instance ToSchema OrgAndLocationResponse
instance ToJSON OrgAndLocationResponse
instance FromJSON OrgAndLocationResponse


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
  { keyInfoId             :: CT.ORKeyId
  , keyInfoUserId         :: UserId  -- TODO: There should be a forien key for Org in here....not sure that user is relevant...
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

data ORError
  = DBErrorORE SqlError
  | UnmatchedUniqueViolationORE SqlError
  -- | The user tried to add a org with the a GS1CompanyPrefix that already exsits.
  | GS1CompanyPrefixExistsORE
  | OrgDoesNotExistORE
  | UserAuthFailureORE (SAS.AuthResult ())
  -- | When adding a user fails for an unknown reason.
  | UserCreationErrorORE String CallStack
  | ORKeyErrorORE ORKeyError
  | LocationNotKnownORE
  | LocationExistsORE
  | UnknownUserORE
  | OperationNotPermittedORE GS1CompanyPrefix UserId
  -- | An error that isn't specifically excluded by the types, but that the
  -- developers don't think is possible to hit, or know of a situation which
  -- could cause this case to be exercised.
  | UnexpectedErrorORE CallStack
  deriving (Show, Generic)

data ORKeyError
  = InvalidRSAKeyORKE JWK
  | InvalidRSAKeySizeORKE Expected Received
  | KeyIsPrivateKeyORKE
  | PublicKeyInsertionErrorORKE [CT.ORKeyId]
  | KeyNotFoundORKE CT.ORKeyId
  | UnauthorisedKeyAccessORKE
  | KeyAlreadyRevokedORKE
  | KeyAlreadyExpiredORKE
  -- | If it is detected that the key has a revocation time and no revoking
  -- user or the key has a revoking user but now revoking time. Hopefully in
  -- practice it is not possible to produce this error since it probably
  -- indicates a bug in our code. It is only possible to generate this error
  -- because we don't store the revoking data in the database as a
  -- Maybe (Time, User) because this is technically complex. If we encounter
  -- this error it might be a good time to re-evaulate whether it is better to
  -- fix the storage datatype so its not possible to generate this error in the
  -- first place.
  | InvalidRevocationORKE (Maybe LocalTime) (Maybe PrimaryKeyType) CallStack
  | AddedExpiredKeyORKE
  deriving (Show)

newtype Bit  = Bit  {getBit :: Int} deriving (Show, Eq, Read, Ord)
newtype Expected = Expected {getExpected :: Bit} deriving (Show, Eq, Read, Ord)
newtype Received = Received {getReceived :: Bit} deriving (Show, Eq, Read, Ord)


-- Lens definitions for Error Types.
$(makeClassyPrisms ''ORError)
$(makeClassyPrisms ''ORKeyError)

instance AsSqlError ORError where _SqlError = _DBErrorORE
instance AsORKeyError ORError where _ORKeyError = _ORKeyErrorORE
