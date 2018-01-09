{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module StorageBeam where

import Control.Lens
import Database.Beam as B
import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Database.Beam.Backend
import Database.Beam.Backend.SQL.BeamExtensions
import Database.PostgreSQL.Simple.FromField
import Database.Beam.Backend.SQL

import Data.Text (Text)
import Data.Int
import Data.Time

import Text.Read

import Data.GS1.EventID
import qualified Data.GS1.Event as Ev
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy

import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Migrate.SQL.Types
import Database.Beam.Migrate.Types

data UserT f = User
  { _userId              :: C f (Auto Int32)
  , _userBizId           :: PrimaryKey BusinessT f
  , _firstName           :: C f Text
  , _lastName            :: C f Text
  , _phoneNumber         :: C f Text
  , _passwordHash        :: C f Text --XXX - should this be blob?
  , _emailAddress        :: C f Text }
  deriving Generic

maxLen = 120

migrationStorage :: Migration PgCommandSyntax (CheckedDatabaseSettings Postgres SupplyChainDb)
migrationStorage =
  SupplyChainDb
    <$> createTable "users"
    (
      User
          (field "_userId" bigserial)
          (BizId (field "_userBizId" bigserial))
          (field "_firstName" (varchar (Just maxLen)) notNull)
          (field "_lastName" (varchar (Just maxLen)) notNull)
          (field "_phoneNumber" (varchar (Just maxLen)) notNull)
          (field "_passwordHash" (varchar (Just maxLen)) notNull)
          (field "_emailAddress" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "keys"
    (
      Key
          (field "_keyId" bigserial)
          (UserId (field "_userId" bigserial))
          (field "_rsa_n" bigserial)
          (field "_rsa_e" bigserial)
          (field "_creationTime" bigserial)
          (field "_revocationTime" bigserial)
    )
    <*> createTable "businesses"
    (
      Business
          (field "_bizId" bigserial)
          (field "_bizName" (varchar (Just maxLen)) notNull)
          (field "_bizGs1CompanyPrefix" bigserial)
          (field "_bizFunction" (varchar (Just maxLen)) notNull)
          (field "_bizSiteName" (varchar (Just maxLen)) notNull)
          (field "_bizAddress" (varchar (Just maxLen)) notNull)
          (field "_bizLat" double notNull)
          (field "_bizLong" double notNull)
    )
    <*> createTable "contacts"
    (
      Contact
          (field "_contactId" bigserial)
          (UserId (field "_contactUser1" bigserial))
          (UserId (field "_contactUser2" bigserial))
    )
    <*> createTable "labels"
    (
      Label
          (field "_labelId" bigserial)
          (field "_labelGs1CompanyPrefix" (varchar (Just maxLen)) notNull)
          (field "_itemReference" (varchar (Just maxLen)) notNull)
          (field "_serialNumber" (varchar (Just maxLen)) notNull)
          (field "_state" (varchar (Just maxLen)) notNull)
          (field "_labelType" (varchar (Just maxLen)) notNull)
          (field "_lot" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "items"
    (
      Item
          (field "_itemId" bigserial)
          (LabelId (field "_itemLabelId" bigserial))
          (field "_itemDescription" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "transformations"
    (
      Transformation
          (field "_transformationId" bigserial)
          (field "_transformationDescription" (varchar (Just maxLen)) notNull)
          (BizId (field "_transformBizId" bigserial))
    )
    <*> createTable "locations"
    (
      Location
          (field "_locationId" bigserial)
          (BizId (field "_locationBizId" bigserial))
          (field "_locationLat" double)
          (field "_locationLong" double)
    )
    <*> createTable "events"
    (
      Event
          (field "_eventId" bigserial)
          (field "_foreignEventId" (varchar (Just maxLen)) notNull)
          (BizId (field "_eventLabelId" bigserial))
          (WhatId (field "_eventWhatId" bigserial))
          (WhyId (field "_eventWhyId" bigserial))
          (WhereId (field "_eventWhereId" bigserial))
          (WhenId (field "_eventWhenId" bigserial))
          (UserId (field "_eventCreatedBy" bigserial))
          (field "_jsonEvent" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "whats"
    (
      What
          (field "_whatId" bigserial)
          (field "_whatType" bigserial) -- bigserial for now FIXME
          (field "_action" bigserial) -- bigserial for now FIXME
          (LabelId (field "_parent" bigserial)) -- bigserial for now FIXME
          (field "_input" bigserial) -- bigserial for now FIXME
          (field "_output" bigserial) -- bigserial for now FIXME
          (field "_bizTransactionId" bigserial) -- bigserial for now FIXME
          (TransformationId (field "_whatTransformationId" bigserial)) -- bigserial for now FIXME
    )
    <*> createTable "whys"
    (
      Why
          (field "_whyId" bigserial)
          (field "_bizStep" bigserial) -- waiting for the compuler to tell us the type
          (field "_disposition" bigserial) -- waiting for the compuler to tell us the type
    )
    <*> createTable "wheres"
    (
      Where
          (field "_whereId" bigserial)
          (LocationId (field "_readPoint" bigserial))
          (LocationId (field "_bizLocation" bigserial))
          (field "_srcType" bigserial) -- waiting for compiler
          (field "_destType" bigserial) -- waiting for compiler
    )
    <*> createTable "whens"
    (
      When
          (field "_whenId" bigserial)
          (field "_eventTime" bigserial)
          (field "_recordTime" bigserial)
          (field "_timeZone" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "labelEvents"
    (
      LabelEvent
          (field "_labelEventId" bigserial)
          (LabelId (field "_labelEventLabelId" bigserial))
          (EventId (field "_labelEventEventId" bigserial))
    )


type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Show (PrimaryKey UserT Identity)

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f (Auto Int32))
    deriving Generic
  primaryKey = UserId . _userId

data KeyT f = Key
  { _keyId              :: C f (Auto Int32)
  , _keyUserId          :: PrimaryKey UserT f
  , _rsa_n              :: C f Int32 --XXX should this be Int64?
  , _rsa_e              :: C f Int32 -- as above
  , _creationTime       :: C f Int32 --XXX date.. Int64?
  , _revocationTime     :: C f Int32 -- as above
  }
  deriving Generic
type Key = KeyT Identity
type KeyId = PrimaryKey KeyT Identity

deriving instance Show Key

instance Beamable KeyT
instance Beamable (PrimaryKey KeyT)

instance Table KeyT where
  data PrimaryKey KeyT f = KeyId (C f (Auto Int32))
    deriving Generic
  primaryKey = KeyId . _keyId

data BusinessT f = Business
  { _bizId                :: C f (Auto Int32)
  , _bizName              :: C f Text
  , _bizGs1CompanyPrefix  :: C f Int32
  , _bizFunction          :: C f Text
  , _bizSiteName          :: C f Text
  , _bizAddress           :: C f Text
  , _bizLat               :: C f Float
  , _bizLong              :: C f Float }
  deriving Generic
type Business = BusinessT Identity
type BizId = PrimaryKey BusinessT Identity

deriving instance Show Business

instance Beamable BusinessT
instance Beamable (PrimaryKey BusinessT)
deriving instance Show (PrimaryKey BusinessT Identity)

instance Table BusinessT where
  data PrimaryKey BusinessT f = BizId (C f (Auto Int32))
    deriving Generic
  primaryKey = BizId . _bizId

data ContactT f = Contact
  { _contactId                :: C f (Auto Int32)
  , _contactUser1Id           :: PrimaryKey UserT f
  , _contactUser2Id           :: PrimaryKey UserT f }
  deriving Generic

type Contact = ContactT Identity
type ContactId = PrimaryKey ContactT Identity

deriving instance Show Contact

instance Beamable ContactT
instance Beamable (PrimaryKey ContactT)
deriving instance Show (PrimaryKey ContactT Identity)

instance Table ContactT where
  data PrimaryKey ContactT f = ContactId (C f (Auto Int32))
    deriving Generic
  primaryKey = ContactId . _contactId

data LabelT f = Label
  { _labelId                 :: C f (Auto Int32)
  , _labelGs1CompanyPrefix   :: C f Text --should this be bizId instead?
  , _itemReference           :: C f Text
  , _serialNumber            :: C f Text
  , _state                   :: C f Text
  , _labelType               :: C f Text
  , _lot                     :: C f Text }
  deriving Generic
type Label = LabelT Identity
type LabelId = PrimaryKey LabelT Identity

deriving instance Show Label

instance Beamable LabelT
instance Beamable (PrimaryKey LabelT)
deriving instance Show (PrimaryKey LabelT Identity)

instance Table LabelT where
  data PrimaryKey LabelT f = LabelId (C f (Auto Int32))
    deriving Generic
  primaryKey = LabelId . _labelId

data ItemT f = Item
  { _itemId            :: C f (Auto Int32)
  , _itemLabelId       :: PrimaryKey LabelT f
  , _itemDescription   :: C f Text }
  deriving Generic
type Item = ItemT Identity
type ItemId = PrimaryKey ItemT Identity

deriving instance Show Item

instance Beamable ItemT
instance Beamable (PrimaryKey ItemT)
deriving instance Show (PrimaryKey ItemT Identity)

instance Table ItemT where
  data PrimaryKey ItemT f = ItemId (C f (Auto Int32))
    deriving Generic
  primaryKey = ItemId . _itemId

data TransformationT f = Transformation
  { _transformationId           :: C f (Auto Int32)
  , _transformationDescription  :: C f Text
  , _transformBizId             :: PrimaryKey BusinessT f }
  deriving Generic
type Transformation = TransformationT Identity
type TransformationId = PrimaryKey TransformationT Identity

deriving instance Show Transformation

instance Beamable TransformationT
instance Beamable (PrimaryKey TransformationT)
deriving instance Show (PrimaryKey TransformationT Identity)

instance Table TransformationT where
  data PrimaryKey TransformationT f = TransformationId (C f (Auto Int32))
    deriving Generic
  primaryKey = TransformationId . _transformationId

data LocationT f = Location
  { _locationId                 :: C f (Auto Int32)
  , _locationBizId              :: PrimaryKey BusinessT f
  , _locationLat                :: C f Float
  , _locationLong               :: C f Float }
  deriving Generic

type Location = LocationT Identity
type LocationId = PrimaryKey LocationT Identity

deriving instance Show Location

instance Beamable LocationT
instance Beamable (PrimaryKey LocationT)
deriving instance Show (PrimaryKey LocationT Identity)

instance Table LocationT where
  data PrimaryKey LocationT f = LocationId (C f (Auto Int32))
    deriving Generic
  primaryKey = LocationId . _locationId

data EventT f = Event
  { _eventId                    :: C f (Auto Int32)
  , _foreignEventId             :: C f Text
  , _eventLabelId               :: PrimaryKey BusinessT f --the label scanned to generate this event.
  , _eventWhatId                :: PrimaryKey WhatT f
  , _eventWhyId                 :: PrimaryKey WhyT f
  , _eventWhereId               :: PrimaryKey WhereT f
  , _eventWhenId                :: PrimaryKey WhenT f
  , _eventCreatedBy             :: PrimaryKey UserT f
  , _jsonEvent                  :: C f Text }
  deriving Generic
type Event = EventT Identity
type EventId = PrimaryKey EventT Identity

deriving instance Show Event

instance Beamable EventT
instance Beamable (PrimaryKey EventT)
deriving instance Show (PrimaryKey EventT Identity)

instance Table EventT where
  data PrimaryKey EventT f = EventId (C f (Auto Int32))
    deriving Generic
  primaryKey = EventId . _eventId

-- isn't EventType already defined in GS1Combinators/src/.../Event.hs?
-- data EventType = ObjectEvent
--                | AggregationEvent
--                | TransactionEvent
--                | TransformationEvent
--                  deriving (Show, Enum, Read)
-- fromField instance

data WhatT f = What
  { _whatId                     :: C f (Auto Int32)
  , _whatType                   :: C f Ev.EventType
  , _action                     :: C f Action
  , _parent                     :: PrimaryKey LabelT f
  , _input                      :: C f [LabelEPC]
  , _output                     :: C f [LabelEPC]
  , _bizTransactionId           :: C f Int32 -- probably link to a table of biztransactions
  , _whatTransformationId       :: PrimaryKey TransformationT f }
  deriving Generic

type What = WhatT Identity
type WhatId = PrimaryKey WhatT Identity

deriving instance Show What
instance Beamable WhatT

instance Beamable (PrimaryKey WhatT)
deriving instance Show (PrimaryKey WhatT Identity)

instance Table WhatT where
  data PrimaryKey WhatT f = WhatId (C f (Auto Int32))
    deriving Generic
  primaryKey = WhatId . _whatId

data WhyT f = Why
  { _whyId                      :: C f (Auto Int32)
  , _bizStep                    :: C f BizStep
  , _disposition                :: C f Disposition }
  deriving Generic

type Why = WhyT Identity
type WhyId = PrimaryKey WhyT Identity
deriving instance Show Why
instance Beamable WhyT
instance Beamable (PrimaryKey WhyT)
deriving instance Show (PrimaryKey WhyT Identity)

instance Table WhyT where
  data PrimaryKey WhyT f = WhyId (C f (Auto Int32))
    deriving Generic
  primaryKey = WhyId . _whyId

data WhereT f = Where
  { _whereId                    :: C f (Auto Int32)
  , _readPoint                  :: PrimaryKey LocationT f
  , _bizLocation                :: PrimaryKey LocationT f
  , _srcType                    :: C f SourceDestType
  , _destType                   :: C f SourceDestType }
  deriving Generic

type Where = WhereT Identity
type WhereId = PrimaryKey WhereT Identity
deriving instance Show Where
instance Beamable WhereT
instance Beamable (PrimaryKey WhereT)
deriving instance Show (PrimaryKey WhereT Identity)

instance Table WhereT where
  data PrimaryKey WhereT f = WhereId (C f (Auto Int32))
    deriving Generic
  primaryKey = WhereId . _whereId


data WhenT f = When
  { _whenId                      :: C f (Auto Int32)
  , _eventTime                   :: C f Int64
  , _recordTime                  :: C f Int64
  , _timeZone                    :: C f TimeZone }
  deriving Generic

type When = WhenT Identity
type WhenId = PrimaryKey WhenT Identity
deriving instance Show When
instance Beamable WhenT
instance Beamable (PrimaryKey WhenT)
deriving instance Show (PrimaryKey WhenT Identity)

instance Table WhenT where
  data PrimaryKey WhenT f = WhenId (C f (Auto Int32))
    deriving Generic
  primaryKey = WhenId . _whenId

data LabelEventT f = LabelEvent
  { _labelEventId               :: C f (Auto Int32)
  , _labelEventLabelId          :: PrimaryKey LabelT f
  , _labelEventEventId          :: PrimaryKey EventT f }
  deriving Generic

type LabelEvent = LabelEventT Identity
type LabelEventId = PrimaryKey LabelEventT Identity
deriving instance Show LabelEvent
instance Beamable LabelEventT
instance Beamable (PrimaryKey LabelEventT)
deriving instance Show (PrimaryKey LabelEventT Identity)

instance Table LabelEventT where
  data PrimaryKey LabelEventT f = LabelEventId (C f (Auto Int32))
    deriving Generic
  primaryKey = LabelEventId . _labelEventId


data SupplyChainDb f = SupplyChainDb
  { _users           :: f (TableEntity UserT)
  , _keys            :: f (TableEntity KeyT)
  , _businesses      :: f (TableEntity BusinessT)
  , _contacts        :: f (TableEntity ContactT)
  , _labels          :: f (TableEntity LabelT)
  , _items           :: f (TableEntity ItemT)
  , _transformations :: f (TableEntity TransformationT)
  , _locations       :: f (TableEntity LocationT)
  , _events          :: f (TableEntity EventT)
  , _whats           :: f (TableEntity WhatT)
  , _whys            :: f (TableEntity WhyT)
  , _wheres          :: f (TableEntity WhereT)
  , _whens           :: f (TableEntity WhenT)
  , _labelEvents     :: f (TableEntity LabelEventT)
  }
  deriving Generic
instance Database SupplyChainDb

-- instance HasSqlValueSyntax be String => HasSqlValueSyntax be EventType where
--   sqlValueSyntax = autoSqlValueSyntax

-- instance FromField EventType where
--   fromField f mdata = do
--                         x <- readMaybe <$> fromField f mdata
--                         case x of
--                           Nothing -> returnError ConversionFailed f "Could not 'read' value for 'EventType'"
--                           Just x -> pure x

-- instance FromBackendRow Postgres EventType

supplyChainDb :: DatabaseSettings be SupplyChainDb
supplyChainDb = defaultDbSettings
  -- `withDbModification`
  -- dbModification
  --   {
  --     _supplyChainUsers =
  --       modifyTable (const "users") $
  --       tableModification
  --       {
  --         _userBizId = fieldNamed "biz_id"
  --       }
  --   , _supplyChainKeys =
  --       modifyTable (const "keys") $
  --       tableModification
  --       {
  --         _keyUserId = fieldNamed "user_id"
  --       }
  --   , _supplyChainBusinesses =
  --       modifyTable (const "businesses") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainContact =
  --       modifyTable (const "contacts") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainLabels =
  --       modifyTable (const "labels") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainTransformations =
  --       modifyTable (const "transformations") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainLocations =
  --       modifyTable (const "locations") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainEvents =
  --       modifyTable (const "events") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainWhats =
  --       modifyTable (const "whats") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainWhys =
  --       modifyTable (const "whys") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainWheres =
  --       modifyTable (const "wheres") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainWhens =
  --       modifyTable (const "whens") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   , _supplyChainLabelEvents =
  --       modifyTable (const "labelEvents") $
  --       tableModification {
  --         _someField = fromField "short_name"
  --       }
  --   }
