{-# LANGUAGE TemplateHaskell #-}

module Mirza.BusinessRegistry.Types where



import           Mirza.Common.Types


import           Data.Pool                  as Pool
import           Database.PostgreSQL.Simple (Connection)

import           Crypto.Scrypt              (ScryptParams)

import           Control.Lens.TH

import           Katip                      as K


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
