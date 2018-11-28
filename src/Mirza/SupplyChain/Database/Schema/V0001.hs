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

import           Mirza.Common.Beam                (lastUpdateField)
import           Mirza.Common.GS1BeamOrphans
import qualified Mirza.Common.GS1BeamOrphans      as MU
import           Mirza.Common.Types               hiding (UserId)
import           Mirza.Common.Types               (BRKeyId (..), PrimaryKeyType)

import           Control.Lens

import           Data.Aeson                       (FromJSON, ToJSON)

import           Data.Swagger                     (ToSchema)

import           Data.ByteString                  (ByteString)
import           Data.Text                        (Text)

import           Data.Time                        (LocalTime)

import           Data.UUID                        (UUID)

import           Database.Beam                    as B
import           Database.Beam.Migrate.SQL        (DataType)
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax    (PgDataTypeSyntax)

import           Crypto.JOSE                      (CompactJWS, JWSHeader)

import           Text.Email.Validate              (EmailAddress)

--------------------------------------------------------------------------------
-- Constants and Utils
--------------------------------------------------------------------------------

defaultFieldMaxLength :: Word
defaultFieldMaxLength = 120

-- | Length of the timezone offset
maxTimeZoneLength :: Word
maxTimeZoneLength = 10

pkSerialType :: DataType PgDataTypeSyntax UUID
pkSerialType = uuid

-- Database
data SupplyChainDb f = SupplyChainDb
  { _users            :: f (TableEntity UserT)
  , _businesses       :: f (TableEntity BusinessT)
  , _contacts         :: f (TableEntity ContactT)
  , _labels           :: f (TableEntity LabelT)
  , _what_labels      :: f (TableEntity WhatLabelT)
  , _transformations  :: f (TableEntity TransformationT)
  , _locations        :: f (TableEntity LocationT)
  , _events           :: f (TableEntity EventT)
  , _whats            :: f (TableEntity WhatT)
  , _biz_transactions :: f (TableEntity BizTransactionT)
  , _whys             :: f (TableEntity WhyT)
  , _wheres           :: f (TableEntity WhereT)
  , _whens            :: f (TableEntity WhenT)
  , _label_events     :: f (TableEntity LabelEventT)
  , _user_events      :: f (TableEntity UserEventT)
  , _signatures       :: f (TableEntity SignatureT)
  , _hashes           :: f (TableEntity HashesT)
  , _blockchain       :: f (TableEntity BlockChainT)
  }
  deriving Generic
instance Database be SupplyChainDb


-- Migration: Intialisation -> V1.
migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres SupplyChainDb)
migration () =
  SupplyChainDb
    <$> createTable "users" ( User
          lastUpdateField
          (field "user_id" pkSerialType)
          (BizId (field "user_biz_id" gs1CompanyPrefixType))
          (field "user_first_name" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "user_last_name" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "user_phone_number" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "user_password_hash" binaryLargeObject notNull)
          (field "user_email_address" emailAddressType unique)
    )
    <*> createTable "businesses" ( Business
          lastUpdateField
          (field "biz_gs1_company_prefix" gs1CompanyPrefixType)
          (field "biz_name" (varchar (Just defaultFieldMaxLength)) notNull)
    )
    <*> createTable "contacts" ( Contact
          lastUpdateField
          (field "contact_id" pkSerialType)
          (UserId (field "contact_user1_id" pkSerialType))
          (UserId (field "contact_user2_id" pkSerialType))
    )
    <*> createTable "labels" ( Label
          lastUpdateField
          (field "label_id" pkSerialType)
          (field "label_gs1_company_prefix" gs1CompanyPrefixType notNull)
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
    <*> createTable "what_labels" ( WhatLabel
          lastUpdateField
          (field "what_label_id" pkSerialType)
          (WhatId (field "what_label_what_id" pkSerialType))
          (LabelId (field "what_label_label_id" pkSerialType))
          (field "what_label_label_type" (maybeType labelType))
    )
    <*> createTable "transformations" ( Transformation
          lastUpdateField
          (field "transformation_id" pkSerialType)
          (field "transformation_description" (varchar (Just defaultFieldMaxLength)) notNull)
          (BizId (field "transformation_biz_id" gs1CompanyPrefixType))
    )
    <*> createTable "locations" ( Location
          lastUpdateField
          (field "location_id" locationRefType)
          (BizId (field "location_biz_id" gs1CompanyPrefixType))
          (field "location_function" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "location_site_name" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "location_address" (varchar (Just defaultFieldMaxLength)) notNull)
          -- this needs to be locationReferenceNum
          (field "location_lat" double)
          (field "location_long" double)
    )
    <*> createTable "events" ( Event
          lastUpdateField
          (field "event_id" pkSerialType)
          (field "event_foreign_event_id" (maybeType uuid))
          (UserId (field "event_created_by" pkSerialType))
          (field "event_json" json notNull)
          (field "event_to_sign" bytea notNull unique)
    )
    <*> createTable "whats" ( What
          lastUpdateField
          (field "what_id" pkSerialType)
          (field "what_event_type" (maybeType eventType))
          (field "what_action" (maybeType actionType))
          (LabelId (field "what_parent" (maybeType pkSerialType)))
          (BizTransactionId (field "what_biz_transaction_id" (maybeType pkSerialType)))
          (TransformationId (field "what_transformation_id" (maybeType pkSerialType)))
          (EventId (field "what_event_id" pkSerialType))
    )
    <*> createTable "biz_transactions" ( BizTransaction
          lastUpdateField
          (field "biz_transaction_id" pkSerialType)
          (field "biz_transaction_type_id" (varchar (Just defaultFieldMaxLength)))
          (field "biz_transaction_id_urn" (varchar (Just defaultFieldMaxLength)))
          (EventId (field "biz_transaction_event_id" pkSerialType))
    )
    <*> createTable "whys" ( Why
          lastUpdateField
          (field "why_id" pkSerialType)
          (field "why_biz_step" (maybeType text))
          (field "why_disposition" (maybeType text))
          (EventId (field "why_event_id" pkSerialType))
    )
    <*> createTable "wheres" ( Where
          lastUpdateField
          (field "where_id" pkSerialType)
          (field "where_gs1_company_prefix" gs1CompanyPrefixType notNull)
          (field "where_source_dest_type" (maybeType srcDestType))
          (field "where_gs1_location_id" locationRefType notNull)
          (field "where_location_field" locationType notNull)
          (field "where_sgln_ext" (maybeType sglnExtType))
          (EventId (field "where_event_id" pkSerialType))
    )
    <*> createTable "whens" ( When
          lastUpdateField
          (field "when_id" pkSerialType)
          (field "when_event_time" timestamptz notNull)
          (field "when_record_time" (maybeType timestamptz))
          (field "when_time_zone" (varchar (Just maxTimeZoneLength)) notNull)
          (EventId (field "when_event_id" pkSerialType))
    )
    <*> createTable "label_events" ( LabelEvent
          lastUpdateField
          (field "label_event_id" pkSerialType)
          (LabelId (field "label_event_label_id" pkSerialType))
          (EventId (field "label_event_event_id" pkSerialType))
    )
    <*> createTable "user_event" ( UserEvent
          lastUpdateField
          (field "user_events_id" pkSerialType)
          (EventId (field "user_events_event_id" pkSerialType notNull))
          (UserId (field "user_events_user_id" pkSerialType notNull))
          (field "user_events_has_signed" boolean notNull)
          (UserId (field "user_events_added_by" pkSerialType notNull))
          (field "user_events_signed_hash" (maybeType bytea))
    )
    <*> createTable "signatures" ( Signature
          lastUpdateField
          (field "signature_id" pkSerialType)
          (UserId (field "signature_user_id" pkSerialType notNull))
          (EventId (field "signature_event_id" pkSerialType notNull))
          (field "signature_key_id" brKeyIdType notNull)
          (field "signature_signature" json notNull)
          (field "signature_timestamp" timestamptz notNull)
    )
    <*> createTable "hashes" ( Hashes
          lastUpdateField
          (field "hashes_id" pkSerialType)
          (EventId (field "hashes_event_id" pkSerialType notNull))
          (field "hashes_hash" bytea notNull)
          (field "hashes_is_signed" boolean notNull)
          (UserId (field "hashes_signed_by_user_id" pkSerialType notNull))
          (field "hashes_key_id" brKeyIdType notNull)
    )
    <*> createTable "blockchain" ( BlockChain
          lastUpdateField
          (field "blockchain_id" pkSerialType)
          (EventId (field "blockchain_event_id" pkSerialType notNull))
          (field "blockchain_hash" bytea notNull)
          (field "blockchain_address" text notNull)
          (field "blockchain_foreign_id" int notNull)
    )


--------------------------------------------------------------------------------
-- User table.
--------------------------------------------------------------------------------

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

data UserT f = User
  { user_last_update   :: C f (Maybe LocalTime)
  , user_id            :: C f PrimaryKeyType
  , user_biz_id        :: PrimaryKey BusinessT f
  , user_first_name    :: C f Text
  , user_last_name     :: C f Text
  , user_phone_number  :: C f Text
  , user_password_hash :: C f ByteString --XXX - should this be blob?
  , user_email_address :: C f EmailAddress }
  deriving Generic

deriving instance Show User
deriving instance Show (PrimaryKey UserT Identity)

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = UserId . user_id
deriving instance Eq (PrimaryKey UserT Identity)


--------------------------------------------------------------------------------
-- Business Table
--------------------------------------------------------------------------------

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 11

type Business = BusinessT Identity
deriving instance Show Business

data BusinessT f = Business
  { biz_last_update        :: C f (Maybe LocalTime)
  , biz_gs1_company_prefix :: C f EPC.GS1CompanyPrefix -- PrimaryKey
  , biz_name               :: C f Text }
  deriving Generic

type BizId = PrimaryKey BusinessT Identity
deriving instance Show (PrimaryKey BusinessT Identity)

instance Beamable (PrimaryKey BusinessT)
instance Beamable BusinessT

instance Table BusinessT where
  data PrimaryKey BusinessT f = BizId (C f EPC.GS1CompanyPrefix)
    deriving Generic
  primaryKey = BizId . biz_gs1_company_prefix
deriving instance Eq (PrimaryKey BusinessT Identity)


--------------------------------------------------------------------------------
-- Contact Table
--------------------------------------------------------------------------------

type Contact = ContactT Identity
type ContactId = PrimaryKey ContactT Identity

data ContactT f = Contact
  { contact_last_update :: C f (Maybe LocalTime)
  , contact_id          :: C f PrimaryKeyType
  , contact_user1_id    :: PrimaryKey UserT f
  , contact_user2_id    :: PrimaryKey UserT f }
  deriving Generic

deriving instance Show Contact
deriving instance Show (PrimaryKey ContactT Identity)

instance Beamable ContactT
instance Beamable (PrimaryKey ContactT)

instance Table ContactT where
  data PrimaryKey ContactT f = ContactId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = ContactId . contact_id


--------------------------------------------------------------------------------
-- Label Table
--------------------------------------------------------------------------------

type Label = LabelT Identity
type LabelId = PrimaryKey LabelT Identity

data LabelT f = Label
  { label_last_update        :: C f (Maybe LocalTime)
  , label_id                 :: C f PrimaryKeyType
  , label_gs1_company_prefix :: C f EPC.GS1CompanyPrefix --should this be bizId instead?
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
  , transformation_biz_id      :: PrimaryKey BusinessT f }
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
  , location_biz_id      :: PrimaryKey BusinessT f
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
  , event_created_by       :: PrimaryKey UserT f
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
  , what_biz_transaction_id :: PrimaryKey BizTransactionT (Nullable f)
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
-- Business Transaction Table
--------------------------------------------------------------------------------

type BizTransaction = BizTransactionT Identity
type BizTransactionId = PrimaryKey BizTransactionT Identity

data BizTransactionT f = BizTransaction
  { biz_transaction_last_update :: C f (Maybe LocalTime)
  , biz_transaction_id          :: C f PrimaryKeyType
  , biz_transaction_type_id     :: C f Text
  , biz_transaction_id_urn      :: C f Text
  , biz_transaction_event_id    :: PrimaryKey EventT f }

  deriving Generic

deriving instance Show BizTransaction
deriving instance Show (PrimaryKey BizTransactionT Identity)
deriving instance Show (PrimaryKey BizTransactionT (Nullable Identity))

instance Beamable BizTransactionT
instance Beamable (PrimaryKey BizTransactionT)

instance Table BizTransactionT where
  data PrimaryKey BizTransactionT f = BizTransactionId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = BizTransactionId . biz_transaction_id
deriving instance Eq (PrimaryKey BizTransactionT Identity)


--------------------------------------------------------------------------------
-- Whys Table
--------------------------------------------------------------------------------

type Why = WhyT Identity
type WhyId = PrimaryKey WhyT Identity

data WhyT f = Why
  { why_last_update :: C f (Maybe LocalTime)
  , why_id          :: C f PrimaryKeyType
  , why_biz_step    :: C f (Maybe Text) -- EPC.BizStep
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

data WhereT f = Where
  { where_last_update        :: C f (Maybe LocalTime)
  , where_id                 :: C f PrimaryKeyType
  , where_gs1_company_prefix :: C f EPC.GS1CompanyPrefix
  , where_source_dest_type   :: C f (Maybe EPC.SourceDestType)
  , where_gs1_location_id    :: C f EPC.LocationReference
  , where_location_field     :: C f MU.LocationField
  , where_sgln_ext           :: C f (Maybe EPC.SGLNExtension)
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
  , label_event_event_id    :: PrimaryKey EventT f }
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
-- User Events Table
--------------------------------------------------------------------------------

type UserEvent = UserEventT Identity
type UserEventId = PrimaryKey UserEventT Identity

data UserEventT f = UserEvent
  { user_events_last_update :: C f (Maybe LocalTime)
  , user_events_id          :: C f PrimaryKeyType
  , user_events_event_id    :: PrimaryKey EventT f
  , user_events_user_id     :: PrimaryKey UserT f
  , user_events_has_signed  :: C f Bool
  , user_events_owner       :: PrimaryKey UserT f
  , user_events_signed_hash :: C f (Maybe ByteString)
  }
  deriving Generic

deriving instance Show UserEvent
deriving instance Show (PrimaryKey UserEventT Identity)

instance Beamable UserEventT
instance Beamable (PrimaryKey UserEventT)

instance Table UserEventT where
  data PrimaryKey UserEventT f = UserEventId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = UserEventId . user_events_id


--------------------------------------------------------------------------------
-- Signatures Table
--------------------------------------------------------------------------------

type Signature = SignatureT Identity
type SignatureId = PrimaryKey SignatureT Identity

data SignatureT f = Signature
  { signature_last_update :: C f (Maybe LocalTime)
  , signature_id          :: C f PrimaryKeyType
  , signature_user_id     :: PrimaryKey UserT f
  , signature_event_id    :: PrimaryKey EventT f
  , signature_key_id      :: C f BRKeyId
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
  { hashes_last_update       :: C f (Maybe LocalTime)
  , hashes_id                :: C f PrimaryKeyType
  , hashes_event_id          :: PrimaryKey EventT f
  , hashes_hash              :: C f ByteString
  , hashes_is_signed         :: C f Bool
  , hashes_signed_by_user_id :: PrimaryKey UserT f
  , hashes_key_id            :: C f BRKeyId
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
