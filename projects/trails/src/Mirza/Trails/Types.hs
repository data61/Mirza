{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Mirza.Trails.Types where


import           Mirza.Common.Time
import           Mirza.Common.Types

import           Data.GS1.EPC               (GS1CompanyPrefix)
import           Data.GS1.EventId           (EventId)

import           Database.PostgreSQL.Simple (Connection, SqlError)

import           Katip                      as K

import           Data.Aeson                 (FromJSON (..), ToJSON (..), object,
                                             withObject, (.:), (.=))

import           Data.Swagger               (ToSchema)

import           Control.Lens               hiding ((.=))

import           Data.Pool                  as Pool
import           Data.Text
import           Data.Time                  (UTCTime)
import           GHC.Generics               (Generic)


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
-- Service Response Types
-- *****************************************************************************

-- Note: The definitions in this section are reverse order defined(more specific
--       to more general rather then overview to more detail) because the
--       template haskell used defines that they must be ordered in this way, so
--       they are grouped by theme and commented as such, but the order within
--       a section appears logically bottom to top, rather then the normal top
--       to bottom.

data TrailEntryResponse = TrailEntryResponse
  { trailEntryResponseVersion          :: Integer
  , trailEntryResponseTimestamp        :: EntryTime
  , trailEntryResponseGS1CompanyPrefix :: GS1CompanyPrefix
  , trailEntryResponseEventID          :: EventId
  , trailEntryResponseParentSignatures :: [SignaturePlaceholder]
  , trailEntryResponseSignature        :: SignaturePlaceholder
  } deriving (Show, Generic, Eq)
instance ToSchema TrailEntryResponse

instance ToJSON TrailEntryResponse where
  toJSON (TrailEntryResponse version timestamp org eventId parentSignatures eventSignature) = object
    [ trailEntryResponseJSONFieldVersion          .= version
    , trailEntryResponseJSONFieldTimestamp        .= timestamp
    , trailEntryResponseJSONFieldGS1CompanyPrefix .= org
    , trailEntryResponseJSONFieldEventId          .= eventId
    , trailEntryResponseJSONFieldParentSignatures .= parentSignatures
    , trailEntryResponseJSONFieldSignature        .= eventSignature
    ]
instance FromJSON TrailEntryResponse where
  parseJSON = withObject "TrailEntryResponse" $ \o -> TrailEntryResponse
    <$> o .: trailEntryResponseJSONFieldVersion
    <*> o .: trailEntryResponseJSONFieldTimestamp
    <*> o .: trailEntryResponseJSONFieldGS1CompanyPrefix
    <*> o .: trailEntryResponseJSONFieldEventId
    <*> o .: trailEntryResponseJSONFieldParentSignatures
    <*> o .: trailEntryResponseJSONFieldSignature

trailEntryResponseJSONFieldVersion :: Text
trailEntryResponseJSONFieldVersion = "version"
trailEntryResponseJSONFieldTimestamp :: Text
trailEntryResponseJSONFieldTimestamp = "timestamp"
trailEntryResponseJSONFieldGS1CompanyPrefix :: Text
trailEntryResponseJSONFieldGS1CompanyPrefix = "org"
trailEntryResponseJSONFieldEventId :: Text
trailEntryResponseJSONFieldEventId = "event_id"
trailEntryResponseJSONFieldParentSignatures :: Text
trailEntryResponseJSONFieldParentSignatures = "parent_signatures"
trailEntryResponseJSONFieldSignature :: Text
trailEntryResponseJSONFieldSignature = "signature"



newtype EntryTime = EntryTime {getEntryTime :: UTCTime}
  deriving (Show, Eq, Generic, Read, Ord)
instance FromJSON EntryTime where
  parseJSON = fmap EntryTime . parseJSON
instance ToJSON EntryTime where
  toJSON = toJSON . getEntryTime
instance ToSchema EntryTime
instance DBTimestamp EntryTime where
  toDbTimestamp (EntryTime t) = toLocalTime t
instance ModelTimestamp EntryTime where
  fromDbTimestamp = onLocalTime EntryTime


newtype SignaturePlaceholder = SignaturePlaceholder {getSignature :: Text}
  deriving (Show, Eq, Generic, Read, Ord)
instance FromJSON SignaturePlaceholder where
  parseJSON = fmap SignaturePlaceholder . parseJSON
instance ToJSON SignaturePlaceholder where
  toJSON = toJSON . getSignature
instance ToSchema SignaturePlaceholder



-- *****************************************************************************
-- Error Types
-- *****************************************************************************

data TrailsError
  = DBErrorTE SqlError
  | UnmatchedUniqueViolationTE SqlError
  deriving (Show)
$(makeClassyPrisms ''TrailsError)

instance AsSqlError TrailsError where _SqlError = _DBErrorTE
