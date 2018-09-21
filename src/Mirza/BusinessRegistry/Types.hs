{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Mirza.BusinessRegistry.Types (
    module Mirza.BusinessRegistry.Types
  , module CT
  ) where

import           Mirza.Common.Time                      (CreationTime,
                                                         ExpirationTime,
                                                         RevocationTime)
import           Mirza.Common.Types                     as CT
import           Mirza.Common.Utils

import qualified Mirza.BusinessRegistry.Database.Schema as Schema

import           Data.GS1.EPC                           as EPC

import           Data.Pool                              as Pool
import           Database.Beam                          as B
import           Database.PostgreSQL.Simple             (Connection, SqlError)

import           Crypto.Scrypt                          (ScryptParams)

import           Control.Lens.TH

import           Katip                                  as K

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import           Data.Text                              (Text)
import           Data.Time                              (LocalTime)

import           GHC.Generics                           (Generic)
import           GHC.Stack                              (CallStack)

import           Servant                                (FromHttpApiData (..))


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
instance FromHttpApiData AuthUser where
  parseUrlPiece = notImplemented


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

newtype PEM_RSAPubKey = PEM_RSAPubKey Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance ToSchema PEM_RSAPubKey
instance ToParamSchema PEM_RSAPubKey
instance FromHttpApiData PEM_RSAPubKey where
  parseUrlPiece t = fmap PEM_RSAPubKey (parseUrlPiece t)


data KeyInfoResponse = KeyInfoResponse
  { keyInfoId             :: CT.BRKeyId
  , keyInfoUserId         :: UserId  -- TODO: There should be a forien key for Business in here....not sure that user is relevant...
  , keyInfoState          :: KeyState
  , keyInfoCreationTime   :: CreationTime
  , keyInfoRevocation     :: Maybe (RevocationTime, UserId)
  , keyInfoExpirationTime :: Maybe ExpirationTime
  , keyInfoPEMString      :: PEM_RSAPubKey
  }
  deriving (Generic, Show, Eq)
$(deriveJSON defaultOptions ''KeyInfoResponse)
instance ToSchema KeyInfoResponse


-- *****************************************************************************
-- Error Types
-- *****************************************************************************

data BusinessRegistryError
  = DBErrorBRE SqlError
  -- | The user tried to add a business with the a GS1CompanyPrefix that already exsits.
  | GS1CompanyPrefixExistsBRE
  | BusinessDoesNotExistBRE
  | UserCreationErrorBRE String
  | KeyErrorBRE KeyError
  -- | An error that isn't specifically excluded by the types, but that the
  -- | developers don't think is possible to hit, or know of a situation which
  -- | could cause this case to be excercised.
  | UnexpectedErrorBRE CallStack
  deriving (Show, Generic)

data KeyError
  = InvalidRSAKey PEM_RSAPubKey
  | InvalidRSAKeySize Expected Received
  | PublicKeyInsertionError [CT.BRKeyId]
  | KeyNotFound CT.BRKeyId
  | UnauthorisedKeyAccess
  | KeyAlreadyRevoked
  | KeyAlreadyExpired
  -- | If it is detected that the key has a revocation time and no revoking
  -- user or the key has a revoking user but now revoking time. Hopefully in
  -- practice it is not possible to produce this error since it probably
  -- indicates a bug in our code. It is only possible to generate this error
  -- because we don't store the revoking data in the database as a
  -- Maybe (Time, User) because this is technically complex. If we encounter
  -- this error it might be a good time to re-evaulate whether it is better to
  -- fix the storage datatype so its not possible to generate this error in the
  -- first place.
  | InvalidRevocation (Maybe LocalTime) (PrimaryKey Schema.UserT (Nullable Identity)) CallStack
  deriving (Show)

newtype Bit  = Bit  {getBit :: Int} deriving (Show, Eq, Read, Ord)
newtype Expected = Expected {getExpected :: Bit} deriving (Show, Eq, Read, Ord)
newtype Received = Received {getReceived :: Bit} deriving (Show, Eq, Read, Ord)


-- Lens definitions for Error Types.
$(makeClassyPrisms ''BusinessRegistryError)
$(makeClassyPrisms ''KeyError)

instance AsSqlError BusinessRegistryError where _SqlError = _DBErrorBRE
instance AsKeyError BusinessRegistryError where _KeyError = _KeyErrorBRE
