{-# LANGUAGE TemplateHaskell #-}


module Mirza.Trails.Types where


import           Mirza.Common.Types

import           Database.PostgreSQL.Simple (Connection, SqlError)

import           Katip                      as K

import           Control.Lens               hiding ((.=))

import           Data.Pool                  as Pool


-- *****************************************************************************
-- Context Types
-- *****************************************************************************

data TrailsContext = TrailsContext
  { _trailsEnvType          :: EnvType
  , _trailsDbConnPool       :: Pool Connection
  , _trailsKatipLogEnv      :: K.LogEnv
  , _trailsKatipLogContexts :: K.LogContexts
  , _trailsKatipNamespace   :: K.Namespace
  }
$(makeLenses ''TrailsContext)

instance HasEnvType (TrailsContext) where
  envType = trailsEnvType
instance HasConnPool (TrailsContext) where
  connPool = trailsDbConnPool
instance HasKatipLogEnv (TrailsContext) where
  katipLogEnv = trailsKatipLogEnv
instance HasKatipContext (TrailsContext) where
  katipContexts = trailsKatipLogContexts
  katipNamespace = trailsKatipNamespace


-- *****************************************************************************
-- Error Types
-- *****************************************************************************

data ORError
  = DBErrorORE SqlError
  | UnmatchedUniqueViolationORE SqlError
  deriving (Show)
$(makeClassyPrisms ''ORError)

instance AsSqlError ORError where _SqlError = _DBErrorORE
