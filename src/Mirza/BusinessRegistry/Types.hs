{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Mirza.BusinessRegistry.Types where



import           Mirza.Common.Types
import           Mirza.Common.Utils

import           Mirza.BusinessRegistry.Database.Schema

import           Data.GS1.EPC                           as EPC


import           Data.Pool                              as Pool
import           Database.PostgreSQL.Simple             (Connection, SqlError)

import           Crypto.Scrypt                          (ScryptParams)

import           Control.Lens.TH

import           Katip                                  as K

import           Data.Aeson
import           Data.Swagger
import           Data.Text                              (Text)
import           Data.Time                              (UTCTime)
import           Data.UUID                              (UUID)


import           GHC.Generics                           (Generic)
import           Servant                                (FromHttpApiData (..))



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

  -- Stubs for now...
newtype KeyID = KeyID UUID
  deriving (Show, Eq, Generic, ToJSON, FromJSON)
instance ToSchema KeyID
instance ToParamSchema KeyID
instance FromHttpApiData KeyID where
  parseUrlPiece t = fmap KeyID (parseUrlPiece t)

newtype ExpirationTime = ExpirationTime {unExpirationTime :: UTCTime}
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema ExpirationTime
instance ToParamSchema ExpirationTime
instance FromHttpApiData ExpirationTime where
  parseUrlPiece t = fmap ExpirationTime (parseUrlPiece t)

newtype PEM_RSAPubKey = PEM_RSAPubKey Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
instance ToSchema PEM_RSAPubKey
instance ToParamSchema PEM_RSAPubKey
instance FromHttpApiData PEM_RSAPubKey where
  parseUrlPiece t = fmap PEM_RSAPubKey (parseUrlPiece t)

data BusinessResponse = BusinessResponse {
  bizID    :: EPC.GS1CompanyPrefix,
  bizName  :: Text,
  function :: Text,
  siteName :: Text,
  address  :: Text,
  lat      :: Double,
  lng      :: Double
  }
  deriving (Generic)
instance ToSchema BusinessResponse
instance ToJSON BusinessResponse
instance FromJSON BusinessResponse

data KeyInfo = KeyInfo
  { keyInfoUserId  :: UserID
  , creationTime   :: CreationTime
  , revocationTime :: Maybe RevocationTime
  , keyState       :: KeyState
  , expirationTime :: Maybe ExpirationTime
  }
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema KeyInfo
instance ToParamSchema KeyInfo
instance FromHttpApiData KeyInfo where
  parseUrlPiece t = fmap KeyInfo (parseUrlPiece t)


newtype AuthUser = AuthUser {
  userId        :: UserID
  }
  deriving (Generic)
instance ToSchema AuthUser
instance ToParamSchema AuthUser
instance FromHttpApiData AuthUser where
  parseUrlPiece = notImplemented



data BusinessRegistryError
  = DBErrorBRE SqlError
  | BusinessCreationErrorBRE String
  | UserCreationErrorBRE String
  | KeyErrorBRE KeyError
  deriving (Show, Eq, Generic)


newtype Bit  = Bit  {unBit :: Int} deriving (Show, Eq, Read, Ord)
newtype Expected = Expected {unExpected :: Bit} deriving (Show, Eq, Read, Ord)
newtype Received = Received {unReceived :: Bit} deriving (Show, Eq, Read, Ord)


data KeyError
  = InvalidRSAKey PEM_RSAPubKey
  | InvalidRSAKeySize Expected Received
  | PublicKeyInsertionError [KeyId]
  | KeyNotFound KeyID
  deriving (Show, Eq)




$(makeClassyPrisms ''BusinessRegistryError)
$(makeClassyPrisms ''KeyError)


instance AsSqlError BusinessRegistryError where  _SqlError = _DBErrorBRE
instance AsKeyError BusinessRegistryError where _KeyError = _KeyErrorBRE
