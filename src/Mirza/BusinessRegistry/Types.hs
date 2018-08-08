{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Mirza.BusinessRegistry.Types (
    module Mirza.BusinessRegistry.Types
  , module CT
  ) where

import           Mirza.Common.Time          (CreationTime, ExpirationTime,
                                             RevocationTime)
import           Mirza.Common.Types         as CT
import           Mirza.Common.Utils

import           Data.GS1.EPC               as EPC


import           Data.Pool                  as Pool
import           Database.PostgreSQL.Simple (Connection, SqlError)

import           Crypto.Scrypt              (ScryptParams)

import           Control.Lens.TH

import           Katip                      as K

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger
import           Data.Text                  (Text)

import           GHC.Generics               (Generic)
import           Servant                    (FromHttpApiData (..))



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
  -- , port    :: Word16
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


data User = User {
  userId        :: UserId,
  userFirstName :: Text,
  userLastName  :: Text
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''User)
instance ToSchema User

-- | Note that BusinessRegistry.NewUser is expected to become different in the
-- future, and hence this duplication
data NewUser = NewUser {
  newUserPhoneNumber  :: Text,
  newUserEmailAddress :: EmailAddress,
  newUserFirstName    :: Text,
  newUserLastName     :: Text,
  newUserCompany      :: GS1CompanyPrefix,
  newUserPassword     :: Text
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


-- Business Response Types:
data BusinessResponse = BusinessResponse {
  bizId   :: EPC.GS1CompanyPrefix,
  bizName :: Text
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
  { keyInfoId             :: CT.KeyId
  , keyInfoUserId         :: UserId  -- TODO: There should be a forien key for Business in here....not sure that user is relevant...
  , keyInfoState          :: KeyState
  , keyInfoCreationTime   :: CreationTime
  , keyInfoRevocationTime :: Maybe RevocationTime
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
  | BusinessCreationErrorBRE String
  | UserCreationErrorBRE String
  | KeyErrorBRE KeyError
  deriving (Show, Eq, Generic)

data KeyError
  = InvalidRSAKey PEM_RSAPubKey
  | InvalidRSAKeySize Expected Received
  | PublicKeyInsertionError [CT.KeyId]
  | KeyNotFound CT.KeyId
  | UnauthorisedKeyAccess
  | KeyAlreadyRevoked
  deriving (Show, Eq)

newtype Bit  = Bit  {unBit :: Int} deriving (Show, Eq, Read, Ord)
newtype Expected = Expected {unExpected :: Bit} deriving (Show, Eq, Read, Ord)
newtype Received = Received {unReceived :: Bit} deriving (Show, Eq, Read, Ord)


-- Lens definitions for Error Types.
$(makeClassyPrisms ''BusinessRegistryError)
$(makeClassyPrisms ''KeyError)

instance AsSqlError BusinessRegistryError where _SqlError = _DBErrorBRE
instance AsKeyError BusinessRegistryError where _KeyError = _KeyErrorBRE
