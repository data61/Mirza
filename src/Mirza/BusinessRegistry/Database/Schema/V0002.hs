{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveGeneric         #-}



module Mirza.BusinessRegistry.Database.Schema.V0002
 ( module Mirza.BusinessRegistry.Database.Schema.V0002
 , module V0001

 ) where

import qualified Data.GS1.EPC                     as EPC
import           Mirza.BusinessRegistry.Types
import           Mirza.Common.GS1BeamOrphans
import           Mirza.Common.Types               (PrimaryKeyType)

import           Control.Lens
import           Data.Text                        (Text)

import           Database.Beam                    as B
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres

import           Data.Aeson
import           Data.Swagger
import           Servant (ToHttpApiData(toUrlPiece), FromHttpApiData)

import           GHC.Generics (Generic)

import qualified Mirza.BusinessRegistry.Database.Schema.V0001 as V0001
import           Mirza.BusinessRegistry.Database.Schema.V0001 as V0001' hiding (BusinessRegistryDB(..), migration)



-- Database
data BusinessRegistryDB f = BusinessRegistryDB
  { _users      :: f (TableEntity V0001.UserT)
  , _businesses :: f (TableEntity V0001.BusinessT)
  , _keys       :: f (TableEntity V0001.KeyT)
  , _locations  :: f (TableEntity       LocationT)
  }
  deriving Generic
instance Database anybackend BusinessRegistryDB


migration :: CheckedDatabaseSettings Postgres V0001.BusinessRegistryDB
          -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres BusinessRegistryDB)
migration v0001 = BusinessRegistryDB
  <$> preserve (V0001._users      v0001)
  <*> preserve (V0001._businesses v0001)
  <*> preserve (V0001._keys       v0001)
  <*> createTable "location"
        (LocationT
          (field "location_id" V0001.pkSerialType)
          (V0001.BizId (field "location_biz_id" gs1CompanyPrefixType))
          (field "location_gln" locationEPCType)
          (field "Location_lat" (maybeType latitudeType))
          (field "Location_lon" (maybeType longitudeType))
          (field "location_address" (maybeType $ varchar Nothing))
        )

type Location = LocationT Identity
deriving instance Show Location

data LocationT f = LocationT
  { location_id        :: C f PrimaryKeyType
  , location_biz_id    :: PrimaryKey V0001.BusinessT f
  , location_gln       :: C f EPC.LocationEPC
  , location_latitude  :: C f (Maybe Latitude)
  , location_longitude :: C f (Maybe Longitude)
  , location_address   :: C f (Maybe Text)
  }
  deriving Generic

type LocationId = PrimaryKey LocationT Identity
deriving instance Show (PrimaryKey LocationT Identity)
instance ToSchema LocationId
instance ToParamSchema LocationId
instance ToJSON (PrimaryKey LocationT Identity) where
  toJSON (LocationId uid) = toJSON uid
instance FromJSON (PrimaryKey LocationT Identity) where
  parseJSON = fmap LocationId . parseJSON

instance Beamable LocationT
instance Beamable (PrimaryKey LocationT)

instance Table LocationT where
  data PrimaryKey LocationT f = LocationId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = LocationId . location_id
deriving instance Eq (PrimaryKey LocationT Identity)

instance ToHttpApiData (PrimaryKey LocationT Identity) where
  toUrlPiece (LocationId uuid) = toUrlPiece uuid

instance FromHttpApiData (PrimaryKey LocationT Identity) where
