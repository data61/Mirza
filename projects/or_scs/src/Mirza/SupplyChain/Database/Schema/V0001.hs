{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- | This module contains all the table definitions
-- Convention: Table types and constructors are suffixed with T (for Table).
module Mirza.SupplyChain.Database.Schema.V0001 where

import qualified Data.GS1.EPC                     as EPC
import qualified Data.GS1.Event                   as Ev

import           Mirza.Common.Beam                (defaultFkConstraint,
                                                   lastUpdateField, textType)
import           Mirza.Common.GS1BeamOrphans
import qualified Mirza.Common.GS1BeamOrphans      as MU
import           Mirza.Common.Types
import           Mirza.Common.Types               (ORKeyId (..), PrimaryKeyType)

import           Control.Lens

import           Data.Aeson                       (FromJSON, ToJSON)

import           Data.Swagger                     (ToSchema)

import           Data.ByteString                  (ByteString)
import           Data.Text                        (Text)

import           Data.Time                        (LocalTime)

import           Data.UUID                        (UUID)

import           Database.Beam                    as B
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types      (CheckedDatabaseSettings,
                                                   Migration)
import           Database.Beam.Postgres           (PgJSON, Postgres, bytea,
                                                   json, text)
import           Database.Beam.Postgres.Migrate   (uuid)
import           Database.Beam.Query.DataTypes    (DataType (..))

import           Crypto.JOSE                      (CompactJWS, JWSHeader)

--------------------------------------------------------------------------------
-- Constants and Utils
--------------------------------------------------------------------------------

defaultFieldMaxLength :: Word
defaultFieldMaxLength = 120

-- | Length of the timezone offset
maxTimeZoneLength :: Word
maxTimeZoneLength = 10

-- Database
data SupplyChainDb f = SupplyChainDb
  { _orgs             :: f (TableEntity OrgT)
  , _labels           :: f (TableEntity LabelT)
  , _transformations  :: f (TableEntity TransformationT)
  , _events           :: f (TableEntity EventT)
  , _org_transactions :: f (TableEntity OrgTransactionT)
  , _whats            :: f (TableEntity WhatT)
  , _what_labels      :: f (TableEntity WhatLabelT)
  , _locations        :: f (TableEntity LocationT)
  , _whys             :: f (TableEntity WhyT)
  , _wheres           :: f (TableEntity WhereT)
  , _whens            :: f (TableEntity WhenT)
  , _label_events     :: f (TableEntity LabelEventT)
  , _signatures       :: f (TableEntity SignatureT)
  , _hashes           :: f (TableEntity HashesT)
  , _blockchain       :: f (TableEntity BlockChainT)
  }
  deriving Generic
instance Database be SupplyChainDb

-- Migration: Intialisation -> V1.
migration :: () -> Migration Postgres (CheckedDatabaseSettings Postgres SupplyChainDb)
migration () =
  SupplyChainDb
    <$> createTable "orgs" ( Org
          lastUpdateField
          (field "org_gs1_company_prefix" gs1CompanyPrefixFieldType)
          (field "org_name" (varchar (Just defaultFieldMaxLength)) notNull)
    )
    <*> createTable "labels" ( Label
          lastUpdateField
          (field "label_id" pkSerialType)
          (field "label_gs1_company_prefix" gs1CompanyPrefixFieldType notNull)
          (field "label_item_reference" (maybeType itemRefType))
          (field "label_serial_number" (maybeType serialNumType))
          (field "label_state" (maybeType $ varchar (Just defaultFieldMaxLength)))
          (field "label_lot" (maybeType lotType))
          (field "label_sgtin_filter_value" (maybeType sgtinFilterValue))
          (field "label_asset_type" (maybeType assetType))
          (field "label_quantity_amount" (maybeType amountType))
          (field "label_quantity_uom" (maybeType uomType))
          (field "label_urn" labelEpcUrnType notNull unique)
    )
    <*> createTable "transformations" ( Transformation
          lastUpdateField
          (field "transformation_id" pkSerialType)
          (field "transformation_description" (varchar (Just defaultFieldMaxLength)) notNull)
          (OrgId $ field "transformation_org_id" gs1CompanyPrefixFieldType (defaultFkConstraint "orgs" ["org_gs1_company_prefix"]))
    )
    <*> createTable "events" ( Event
          lastUpdateField
          (field "event_id" pkSerialType)
          (field "event_foreign_event_id" (maybeType uuid))
          (field "event_json" json notNull)
          (field "event_to_sign" bytea notNull unique)
    )
    <*> createTable "org_transactions" ( OrgTransaction
          lastUpdateField
          (field "org_transaction_id" pkSerialType)
          (field "org_transaction_type_id" (varchar (Just defaultFieldMaxLength)))
          (field "org_transaction_id_urn" (varchar (Just defaultFieldMaxLength)))
          (EventId $ field "org_transaction_event_id" pkSerialType (defaultFkConstraint "events" ["event_id"]))
    )
    <*> createTable "whats" ( What
          lastUpdateField
          (field "what_id" pkSerialType)
          (field "what_event_type" (maybeType eventType))
          (field "what_action" (maybeType actionType))
          (LabelId $ field "what_parent" (maybeType pkSerialType) (defaultFkConstraint "labels" ["label_id"]))
          (OrgTransactionId $ field "what_org_transaction_id" (maybeType pkSerialType))
          (TransformationId $ field "what_transformation_id" (maybeType pkSerialType))
          (EventId $ field "what_event_id" pkSerialType (defaultFkConstraint "events" ["event_id"]))
    )
    <*> createTable "what_labels" ( WhatLabel
          lastUpdateField
          (field "what_label_id" pkSerialType)
          (WhatId $ field "what_label_what_id" pkSerialType (defaultFkConstraint "whats" ["what_id"]))
          (LabelId $ field "what_label_label_id" pkSerialType (defaultFkConstraint "labels" ["label_id"]))
          (field "what_label_label_type" (maybeType labelType))
    )
    <*> createTable "locations" ( Location
          lastUpdateField
          (field "location_id" locationRefType)
          (OrgId $ field "location_org_id" gs1CompanyPrefixFieldType ((defaultFkConstraint "orgs" ["org_gs1_company_prefix"])))
          (field "location_function" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "location_site_name" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "location_address" (varchar (Just defaultFieldMaxLength)) notNull)
          -- this needs to be locationReferenceNum
          (field "location_lat" double)
          (field "location_long" double)
    )
    <*> createTable "whys" ( Why
          lastUpdateField
          (field "why_id" pkSerialType)
          (field "why_org_step" (maybeType text))
          (field "why_disposition" (maybeType text))
          (EventId $ field "why_event_id" pkSerialType (defaultFkConstraint "events" ["event_id"]))
    )
    <*> createTable "wheres" ( Where
          lastUpdateField
          (field "where_id" pkSerialType)
          (field "where_gs1_company_prefix" (maybeType gs1CompanyPrefixFieldType))
          (field "where_source_dest_type" (maybeType srcDestType))
          (field "where_gs1_location_id" (maybeType locationRefType))
          (field "where_location_field" locationType notNull)
          (field "where_sgln_ext" (maybeType sglnExtType))
          (field "where_geo" (maybeType textType))
          (EventId $ field "where_event_id" pkSerialType (defaultFkConstraint "events" ["event_id"]))
    )
    <*> createTable "whens" ( When
          lastUpdateField
          (field "when_id" pkSerialType)
          (field "when_event_time" timestamp notNull)
          (field "when_record_time" (maybeType timestamp))
          (field "when_time_zone" (varchar (Just maxTimeZoneLength)) notNull)
          (EventId $ field "when_event_id" pkSerialType (defaultFkConstraint "events" ["event_id"]))
    )
    <*> createTable "label_events" ( LabelEvent
          lastUpdateField
          (field "label_event_id" pkSerialType)
          (LabelId $ field "label_event_label_id" pkSerialType (defaultFkConstraint "labels" ["label_id"]))
          (EventId $ field "label_event_event_id" pkSerialType (defaultFkConstraint "events" ["event_id"]))
          (field "label_event_label_type" (maybeType labelType))
    )
    <*> createTable "signatures" ( Signature
          lastUpdateField
          (field "signature_id" pkSerialType)
          (EventId $ field "signature_event_id" pkSerialType notNull (defaultFkConstraint "events" ["event_id"]))
          (field "signature_key_id" orKeyIdType notNull)
          (field "signature_signature" json notNull)
          (field "signature_timestamp" timestamp notNull)
    )
    <*> createTable "hashes" ( Hashes
          lastUpdateField
          (field "hashes_id" pkSerialType)
          (EventId $ field "hashes_event_id" pkSerialType notNull (defaultFkConstraint "events" ["event_id"]))
          (field "hashes_hash" bytea notNull)
          (field "hashes_is_signed" boolean notNull)
          (field "hashes_key_id" orKeyIdType notNull)
    )
    <*> createTable "blockchain" ( BlockChain
          lastUpdateField
          (field "blockchain_id" pkSerialType)
          (EventId $ field "blockchain_event_id" pkSerialType notNull (defaultFkConstraint "events" ["event_id"]))
          (field "blockchain_hash" bytea notNull)
          (field "blockchain_address" text notNull)
          (field "blockchain_foreign_id" int notNull)
    )

--------------------------------------------------------------------------------
-- Org Table
--------------------------------------------------------------------------------

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 11

type Org = OrgT Identity
deriving instance Show Org

data OrgT f = Org
  { org_last_update        :: C f (Maybe LocalTime)
  , org_gs1_company_prefix :: C f EPC.GS1CompanyPrefix -- PrimaryKey
  , org_name               :: C f Text }
  deriving Generic

type OrgId = PrimaryKey OrgT Identity
deriving instance Show (PrimaryKey OrgT Identity)

instance Beamable (PrimaryKey OrgT)
instance Beamable OrgT

instance Table OrgT where
  data PrimaryKey OrgT f = OrgId (C f EPC.GS1CompanyPrefix)
    deriving Generic
  primaryKey = OrgId . org_gs1_company_prefix
deriving instance Eq (PrimaryKey OrgT Identity)


--------------------------------------------------------------------------------
-- Label Table
--------------------------------------------------------------------------------

type Label = LabelT Identity
type LabelId = PrimaryKey LabelT Identity

data LabelT f = Label
  { label_last_update        :: C f (Maybe LocalTime)
  , label_id                 :: C f PrimaryKeyType
  , label_gs1_company_prefix :: C f EPC.GS1CompanyPrefix --should this be orgId instead?
  , label_item_reference     :: C f (Maybe EPC.ItemReference)
  , label_serial_number      :: C f (Maybe EPC.SerialNumber)
  , label_state              :: C f (Maybe Text)
  , label_lot                :: C f (Maybe EPC.Lot)
  , label_sgtin_filter_value :: C f (Maybe EPC.SGTINFilterValue)
  , label_asset_type         :: C f (Maybe EPC.AssetType)
  , label_quantity_amount    :: C f (Maybe EPC.Amount)
  , label_quantity_uom       :: C f (Maybe EPC.Uom)
  , label_urn                :: C f LabelEPCUrn
  }
  deriving Generic

deriving instance Show Label
deriving instance Show (PrimaryKey LabelT Identity)
deriving instance Show (PrimaryKey LabelT (Nullable Identity))

instance Beamable LabelT
instance Beamable (PrimaryKey LabelT)

instance Table LabelT where
  data PrimaryKey LabelT f = LabelId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = LabelId . label_id
deriving instance Eq (PrimaryKey LabelT Identity)


--------------------------------------------------------------------------------
-- What Label Table
--------------------------------------------------------------------------------

type WhatLabel = WhatLabelT Identity
type WhatLabelId = PrimaryKey WhatLabelT Identity

data WhatLabelT f = WhatLabel
  { what_label_last_update :: C f (Maybe LocalTime)
  , what_label_id          :: C f PrimaryKeyType
  , what_label_what_id     :: PrimaryKey WhatT f
  , what_label_label_id    :: PrimaryKey LabelT f
  , what_label_label_type  :: C f (Maybe MU.LabelType)
  }
  deriving Generic

deriving instance Show WhatLabel
deriving instance Show (PrimaryKey WhatLabelT Identity)

instance Beamable WhatLabelT
instance Beamable (PrimaryKey WhatLabelT)

instance Table WhatLabelT where
  data PrimaryKey WhatLabelT f = WhatLabelId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhatLabelId . what_label_id


--------------------------------------------------------------------------------
-- Transformation Table
--------------------------------------------------------------------------------

type Transformation = TransformationT Identity
type TransformationId = PrimaryKey TransformationT Identity

data TransformationT f = Transformation
  { transformation_last_update :: C f (Maybe LocalTime)
  , transformation_id          :: C f PrimaryKeyType
  , transformation_description :: C f Text
  , transformation_org_id      :: PrimaryKey OrgT f }
  deriving Generic

deriving instance Show Transformation
deriving instance Show (PrimaryKey TransformationT Identity)
deriving instance Show (PrimaryKey TransformationT (Nullable Identity))

instance Beamable TransformationT
instance Beamable (PrimaryKey TransformationT)

instance Table TransformationT where
  data PrimaryKey TransformationT f = TransformationId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = TransformationId . transformation_id
deriving instance Eq (PrimaryKey TransformationT Identity)


--------------------------------------------------------------------------------
-- Location Table
--------------------------------------------------------------------------------

type Location = LocationT Identity
type LocationId = PrimaryKey LocationT Identity

data LocationT f = Location
  { location_last_update :: C f (Maybe LocalTime)
  , location_id          :: C f EPC.LocationReference
  , location_org_id      :: PrimaryKey OrgT f
  , location_site_name   :: C f Text
  , location_address     :: C f Text
  , location_function    :: C f Text
  -- this needs to be locationReferenceNum
  , location_lat         :: C f Double
  , location_long        :: C f Double }
  deriving Generic

deriving instance Show Location
deriving instance Show (PrimaryKey LocationT Identity)

instance Beamable LocationT
instance Beamable (PrimaryKey LocationT)

instance Table LocationT where
  data PrimaryKey LocationT f = LocationId (C f EPC.LocationReference)
    deriving Generic
  primaryKey = LocationId . location_id


--------------------------------------------------------------------------------
-- Events Table
--------------------------------------------------------------------------------

type Event = EventT Identity
type EventId = PrimaryKey EventT Identity

data EventT f = Event
  { event_last_update      :: C f (Maybe LocalTime)
  , event_id               :: C f PrimaryKeyType
  , event_foreign_event_id :: C f (Maybe UUID) -- Event ID from XML from foreign systems.
  , event_json             :: C f (PgJSON Ev.Event)
  , event_to_sign          :: C f ByteString -- this is what users will be given for signing purposes
  }
  deriving Generic

deriving instance Show Event
deriving instance Show (PrimaryKey EventT Identity)
instance ToSchema EventId
instance FromJSON EventId
instance ToJSON EventId

instance Beamable EventT
instance Beamable (PrimaryKey EventT)

-- | Utility function to extract ``eventId``
getEventId :: PrimaryKey EventT f -> C f PrimaryKeyType
getEventId (EventId eventId) = eventId

instance Table EventT where
  data PrimaryKey EventT f = EventId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = EventId . event_id
deriving instance Eq (PrimaryKey EventT Identity)


--------------------------------------------------------------------------------
-- Whats Table
--------------------------------------------------------------------------------

type What = WhatT Identity
type WhatId = PrimaryKey WhatT Identity

data WhatT f = What
  { what_last_update        :: C f (Maybe LocalTime)
  , what_id                 :: C f PrimaryKeyType
  , what_event_type         :: C f (Maybe Ev.EventType)
  , what_action             :: C f (Maybe EPC.Action)
  , what_parent             :: PrimaryKey LabelT (Nullable f)
  , what_org_transaction_id :: PrimaryKey OrgTransactionT (Nullable f)
  , what_transformation_id  :: PrimaryKey TransformationT (Nullable f)
  , what_event_id           :: PrimaryKey EventT f }
  deriving Generic

deriving instance Show What
deriving instance Show (PrimaryKey WhatT Identity)

instance Beamable WhatT
instance Beamable (PrimaryKey WhatT)

instance Table WhatT where
  data PrimaryKey WhatT f = WhatId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhatId . what_id
deriving instance Eq (PrimaryKey WhatT Identity)


--------------------------------------------------------------------------------
-- Org Transaction Table
--------------------------------------------------------------------------------

type OrgTransaction = OrgTransactionT Identity
type OrgTransactionId = PrimaryKey OrgTransactionT Identity

data OrgTransactionT f = OrgTransaction
  { org_transaction_last_update :: C f (Maybe LocalTime)
  , org_transaction_id          :: C f PrimaryKeyType
  , org_transaction_type_id     :: C f Text
  , org_transaction_id_urn      :: C f Text
  , org_transaction_event_id    :: PrimaryKey EventT f }

  deriving Generic

deriving instance Show OrgTransaction
deriving instance Show (PrimaryKey OrgTransactionT Identity)
deriving instance Show (PrimaryKey OrgTransactionT (Nullable Identity))

instance Beamable OrgTransactionT
instance Beamable (PrimaryKey OrgTransactionT)

instance Table OrgTransactionT where
  data PrimaryKey OrgTransactionT f = OrgTransactionId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = OrgTransactionId . org_transaction_id
deriving instance Eq (PrimaryKey OrgTransactionT Identity)


--------------------------------------------------------------------------------
-- Whys Table
--------------------------------------------------------------------------------

type Why = WhyT Identity
type WhyId = PrimaryKey WhyT Identity

data WhyT f = Why
  { why_last_update :: C f (Maybe LocalTime)
  , why_id          :: C f PrimaryKeyType
  , why_org_step    :: C f (Maybe Text) -- EPC.OrgStep
  , why_disposition :: C f (Maybe Text) -- EPC.Disposition
  , why_event_id    :: PrimaryKey EventT f }
  deriving Generic

deriving instance Show Why
deriving instance Show (PrimaryKey WhyT Identity)

instance Beamable WhyT
instance Beamable (PrimaryKey WhyT)

instance Table WhyT where
  data PrimaryKey WhyT f = WhyId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhyId . why_id


--------------------------------------------------------------------------------
-- Wheres Table
--------------------------------------------------------------------------------

type Where = WhereT Identity
type WhereId = PrimaryKey WhereT Identity
type RFC5870Geo = Text

data WhereT f = Where
  { where_last_update        :: C f (Maybe LocalTime)
  , where_id                 :: C f PrimaryKeyType
  , where_gs1_company_prefix :: C f (Maybe EPC.GS1CompanyPrefix)
  , where_source_dest_type   :: C f (Maybe EPC.SourceDestType)
  , where_gs1_location_id    :: C f (Maybe EPC.LocationReference)
  , where_location_field     :: C f MU.LocationField
  , where_sgln_ext           :: C f (Maybe EPC.SGLNExtension)
  , where_geo                :: C f (Maybe RFC5870Geo)
  , where_event_id           :: PrimaryKey EventT f }
  deriving Generic

deriving instance Show Where
deriving instance Show (PrimaryKey WhereT Identity)

instance Beamable WhereT
instance Beamable (PrimaryKey WhereT)

instance Table WhereT where
  data PrimaryKey WhereT f = WhereId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhereId . where_id


--------------------------------------------------------------------------------
-- Whens Table
--------------------------------------------------------------------------------

type When = WhenT Identity
type WhenId = PrimaryKey WhenT Identity
type TzOffsetString = Text

-- EPCISTime is stored as LocalTime with utc timezone.
-- See Mirza.Common.Time for details on how it is done
data WhenT f = When
  { when_last_update :: C f (Maybe LocalTime)
  , when_id          :: C f PrimaryKeyType
  , when_event_time  :: C f LocalTime -- Stored as EPCISTime
  , when_record_time :: C f (Maybe LocalTime) -- Stored as EPCISTime
  , when_time_zone   :: C f TzOffsetString -- TimeZone
  , when_event_id    :: PrimaryKey EventT f }
  deriving Generic

deriving instance Show When
deriving instance Show (PrimaryKey WhenT Identity)

instance Beamable WhenT
instance Beamable (PrimaryKey WhenT)

instance Table WhenT where
  data PrimaryKey WhenT f = WhenId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhenId . when_id


--------------------------------------------------------------------------------
-- Label Events Table
--------------------------------------------------------------------------------

type LabelEvent = LabelEventT Identity
type LabelEventId = PrimaryKey LabelEventT Identity

data LabelEventT f = LabelEvent
  { label_event_last_update :: C f (Maybe LocalTime)
  , label_event_id          :: C f PrimaryKeyType
  , label_event_label_id    :: PrimaryKey LabelT f
  , label_event_event_id    :: PrimaryKey EventT f
  , label_event_label_type  :: C f (Maybe MU.LabelType)
  }
  deriving Generic

deriving instance Show LabelEvent
deriving instance Show (PrimaryKey LabelEventT Identity)

instance Beamable LabelEventT
instance Beamable (PrimaryKey LabelEventT)

instance Table LabelEventT where
  data PrimaryKey LabelEventT f = LabelEventId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = LabelEventId . label_event_id

--------------------------------------------------------------------------------
-- Signatures Table
--------------------------------------------------------------------------------

type Signature = SignatureT Identity
type SignatureId = PrimaryKey SignatureT Identity

data SignatureT f = Signature
  { signature_last_update :: C f (Maybe LocalTime)
  , signature_id          :: C f PrimaryKeyType
  , signature_event_id    :: PrimaryKey EventT f
  , signature_key_id      :: C f ORKeyId
  , signature_signature   :: C f (PgJSON (CompactJWS JWSHeader))
  , signature_timestamp   :: C f LocalTime -- Stored as UTC Time
  }
  deriving Generic

deriving instance Show (PrimaryKey SignatureT Identity)

instance Beamable SignatureT
instance Beamable (PrimaryKey SignatureT)

instance Table SignatureT where
  data PrimaryKey SignatureT f = SignatureId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = SignatureId . signature_id


--------------------------------------------------------------------------------
-- Hashes Table
--------------------------------------------------------------------------------

type Hashes = HashesT Identity
type HashesId = PrimaryKey HashesT Identity

data HashesT f = Hashes
  { hashes_last_update :: C f (Maybe LocalTime)
  , hashes_id          :: C f PrimaryKeyType
  , hashes_event_id    :: PrimaryKey EventT f
  , hashes_hash        :: C f ByteString
  , hashes_is_signed   :: C f Bool
  , hashes_key_id      :: C f ORKeyId
  }
  deriving Generic

deriving instance Show (PrimaryKey HashesT Identity)

instance Beamable HashesT
instance Beamable (PrimaryKey HashesT)

instance Table HashesT where
  data PrimaryKey HashesT f = HashesId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = HashesId . hashes_id


--------------------------------------------------------------------------------
-- Blockchain Table
--------------------------------------------------------------------------------

type BlockChain = BlockChainT Identity
type BlockChainId = PrimaryKey BlockChainT Identity

data BlockChainT f = BlockChain
  { block_chain_last_update :: C f (Maybe LocalTime)
  , blockchain_id           :: C f PrimaryKeyType
  , blockchain_event_id     :: PrimaryKey EventT f
  , blockchain_hash         :: C f ByteString
  , blockchain_address      :: C f Text
  , blockchain_foreign_id   :: C f Integer
  } deriving Generic

deriving instance Show BlockChain
deriving instance Show (PrimaryKey BlockChainT Identity)

instance Beamable BlockChainT
instance Beamable (PrimaryKey BlockChainT)

instance Table BlockChainT where
  data PrimaryKey BlockChainT f = BlockChainId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = BlockChainId . blockchain_id
