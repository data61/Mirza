{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Mirza.BusinessRegistry.Types where



import           Mirza.Common.Types


import           Data.Pool                  as Pool
import           Database.PostgreSQL.Simple (Connection)

import           Crypto.Scrypt              (ScryptParams)

import           Control.Lens.TH

import           Katip                      as K

import           Data.Aeson
import           Data.Swagger
import           GHC.Generics               (Generic)
import           Servant                    (FromHttpApiData, ToHttpApiData)



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
newtype KeyID = KeyID ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema KeyID
instance ToParamSchema KeyID
instance FromHttpApiData KeyID
instance ToHttpApiData KeyID

newtype ExpirationTime = ExpirationTime ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema ExpirationTime
instance ToParamSchema ExpirationTime
instance FromHttpApiData ExpirationTime
instance ToHttpApiData ExpirationTime

newtype PEM_RSAPubKey = PEM_RSAPubKey ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema PEM_RSAPubKey
instance ToParamSchema PEM_RSAPubKey
instance FromHttpApiData PEM_RSAPubKey
instance ToHttpApiData PEM_RSAPubKey

newtype Business = Business ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema Business
instance ToParamSchema Business
instance FromHttpApiData Business
instance ToHttpApiData Business

newtype KeyInfo = KeyInfo ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema KeyInfo
instance ToParamSchema KeyInfo
instance FromHttpApiData KeyInfo
instance ToHttpApiData KeyInfo

newtype User = User ()
  deriving (Generic, ToJSON, FromJSON)
instance ToSchema User
instance ToParamSchema User
instance FromHttpApiData User
instance ToHttpApiData User
