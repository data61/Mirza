{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Mirza.BusinessRegistry.Types where



import           Mirza.Common.Types
import           Mirza.Common.Utils

import           Mirza.BusinessRegistry.StorageBeam

import           Data.Pool                          as Pool
import           Database.PostgreSQL.Simple         (Connection, SqlError)

import           Crypto.Scrypt                      (ScryptParams)

import           Control.Lens.TH

import           Katip                              as K

import           Data.Aeson
import           Data.Swagger
import           GHC.Generics                       (Generic)
import           Servant                            (FromHttpApiData (..))



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


data BussinessRegistryError
  = DBError SqlError
  | KeyNotLargeEnough
  deriving (Show, Eq, Generic)

$(makeClassyPrisms ''BussinessRegistryError)


instance AsSqlError BussinessRegistryError where
  _SqlError = _DBError

  -- Stubs for now...
newtype KeyID = KeyID ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema KeyID
instance ToParamSchema KeyID
instance FromHttpApiData KeyID where
  parseUrlPiece t = fmap KeyID (parseUrlPiece t)

newtype ExpirationTime = ExpirationTime ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema ExpirationTime
instance ToParamSchema ExpirationTime
instance FromHttpApiData ExpirationTime where
  parseUrlPiece t = fmap ExpirationTime (parseUrlPiece t)

newtype PEM_RSAPubKey = PEM_RSAPubKey ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema PEM_RSAPubKey
instance ToParamSchema PEM_RSAPubKey
instance FromHttpApiData PEM_RSAPubKey where
  parseUrlPiece t = fmap PEM_RSAPubKey (parseUrlPiece t)

newtype BusinessResponse = BusinessResponse()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema BusinessResponse
instance ToParamSchema BusinessResponse
instance FromHttpApiData BusinessResponse where
  parseUrlPiece t = fmap BusinessResponse (parseUrlPiece t)

newtype KeyInfo = KeyInfo ()
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
