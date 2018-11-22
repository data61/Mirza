{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE TypeApplications       #-}

module Mirza.BusinessRegistry.Types (
    module Mirza.BusinessRegistry.Types
  , module CT
  ) where

import           Mirza.Common.Time                      (CreationTime,
                                                         ExpirationTime,
                                                         RevocationTime)
import           Mirza.Common.Types                     as CT

import           Data.GS1.EPC                           as EPC

import           Data.Pool                              as Pool

import           Database.Beam
import           Database.Beam.Backend.SQL
import           Database.Beam.Postgres.Syntax (PgDataTypeSyntax)
import           Database.PostgreSQL.Simple             (Connection, SqlError)
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.ToField   (ToField, toField)
import qualified Database.Beam.Migrate      as BMigrate
import qualified Database.Beam.Postgres     as BPostgres

import           Crypto.JOSE                            (JWK)
import           Crypto.Scrypt                          (ScryptParams)

import           Katip                                  as K

import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.TH

import           Data.Swagger
import           Data.Text                              (Text)
import           Data.Time                              (LocalTime)

import           GHC.Generics                           (Generic)
import           GHC.Stack                              (CallStack)

-- *****************************************************************************
-- Context Types
-- *****************************************************************************

data BRContext = BRContext
  { _brEnvType          :: EnvType
  , _brDbConnPool       :: Pool Connection
  , _brScryptPs         :: ScryptParams
  , _brKatipLogEnv      :: K.LogEnv
  , _brKatipLogContexts :: K.LogContexts
  , _brKatipNamespace   :: K.Namespace
  }
$(makeLenses ''BRContext)

instance HasEnvType BRContext where envType = brEnvType
instance HasConnPool BRContext where connPool = brDbConnPool
instance HasScryptParams BRContext where scryptParams = brScryptPs
instance HasKatipLogEnv BRContext where katipLogEnv = brKatipLogEnv
instance HasKatipContext BRContext where
  katipContexts = brKatipLogContexts
  katipNamespace = brKatipNamespace



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
  { newUserEmailAddress :: EmailAddress
  , newUserPassword     :: Text
  , newUserCompany      :: GS1CompanyPrefix
  , newUserFirstName    :: Text
  , newUserLastName     :: Text
  , newUserPhoneNumber  :: Text
  } deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewUser)
instance ToSchema NewUser

-- Auth User Types:
newtype AuthUser = AuthUser { authUserId :: UserId }
  deriving (Show, Eq, Read, Generic)
instance ToSchema AuthUser
instance ToParamSchema AuthUser


data NewBusiness = NewBusiness
  { newBusinessGS1CompanyPrefix :: GS1CompanyPrefix
  , newBusinessName             :: Text
  } deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewBusiness)
instance ToSchema NewBusiness

-- Business Response Types:
data BusinessResponse = BusinessResponse
  { businessGS1CompanyPrefix :: EPC.GS1CompanyPrefix
  , businessName             :: Text
  }
  deriving (Show, Eq, Read, Generic)
instance ToSchema BusinessResponse
instance ToJSON BusinessResponse
instance FromJSON BusinessResponse

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
  { locationId         :: PrimaryKeyType
  , locationGLN        :: EPC.LocationEPC
  , locationBiz        :: GS1CompanyPrefix
  , geoLocId           :: PrimaryKeyType
  , geoLocCoord        :: Maybe (Latitude, Longitude)
  , geoLocAddress      :: Maybe Text
  } deriving (Show, Generic)


instance ToSchema LocationResponse
instance ToJSON LocationResponse
instance FromJSON LocationResponse

data KeyState
  = InEffect -- Can be used
  | Revoked -- Key passed the revocation time
  | Expired -- Key passed the expiration time
  deriving (Show, Eq, Read, Generic)
$(deriveJSON defaultOptions ''KeyState)
instance ToSchema KeyState
instance ToParamSchema KeyState


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
  -- | When adding a user fails with an underlying error arising from the database.
  | UserCreationSQLErrorBRE SqlError
  -- | When adding a user fails for an unknown reason.
  | UserCreationErrorBRE String CallStack
  | BRKeyErrorBRE BRKeyError
  | LocationNotKnownBRE
  | LocationExistsBRE
  -- | An error that isn't specifically excluded by the types, but that the
  -- developers don't think is possible to hit, or know of a situation which
  -- could cause this case to be excercised.
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
