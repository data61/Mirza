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
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy


data UserT f = User
  { _userID              :: C f (Auto Int)
  , _userBizID           :: PrimaryKey BusinessT f
  , _firstName           :: C f Text
  , _lastName            :: C f Text
  , _phoneNumber         :: C f Text
  , _passwordHash        :: C f Text --XXX - should this be blob?
  , _emailAddress        :: C f Text }
  deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Show (PrimaryKey UserT Identity)

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f (Auto Int))
    deriving Generic
  primaryKey = UserId . _userID

data KeyT f = Key
  { _keyID              :: C f (Auto Int32)
  , _keyUserID         :: PrimaryKey UserT f
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
  primaryKey = KeyId . _keyID

data BusinessT f = Business
  { _bizID                :: C f (Auto Int32)
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
  primaryKey = BizId . _bizID

data ContactsT f = Contacts
  { _contactID                :: C f (Auto Int32)
  , _contactUser1ID           :: PrimaryKey UserT f
  , _contactUser2ID           :: PrimaryKey UserT f }
  deriving Generic

type Contacts = ContactsT Identity
type ContactId = PrimaryKey ContactsT Identity

deriving instance Show Contacts

instance Beamable ContactsT
instance Beamable (PrimaryKey ContactsT)
deriving instance Show (PrimaryKey ContactsT Identity)

instance Table ContactsT where
  data PrimaryKey ContactsT f = ContactId (C f (Auto Int32))
    deriving Generic
  primaryKey = ContactId . _contactID

data LabelT f = Label
  { _labelID                 :: C f (Auto Int32)
  , _labelGs1CompanyPrefix  :: C f Text --should this be bizID instead?
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
  primaryKey = LabelId . _labelID

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
  , _transformBizID             :: PrimaryKey BusinessT f }
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
  { _locationID                 :: C f (Auto Int32)
  , _locationBizID              :: PrimaryKey BusinessT f
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
  primaryKey = LocationId . _locationID

data EventT f = Event
  { _eventID                    :: C f (Auto Int32)
  , _foreignEventID             :: C f Text
  , _eventLabelID               :: PrimaryKey BusinessT f --the label scanned to generate this event.
  , _eventWhatID                :: PrimaryKey WhatT f
  , _eventWhyID                 :: PrimaryKey WhyT f
  , _eventWhereID               :: PrimaryKey WhereT f
  , _eventWhenID                :: PrimaryKey WhenT f
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
  primaryKey = EventId . _eventID

data EventType = ObjectEvent
               | AggregationEvent
               | TransactionEvent
               | TransformationEvent
                 deriving (Show, Enum, Read)
-- fromField instance

data WhatT f = What
  { _whatID                     :: C f (Auto Int32)
  , _whatType                   :: C f EventType
  , _action                     :: C f Action
  , _parent                     :: PrimaryKey LabelT f
  , _input                      :: C f [LabelEPC]
  , _output                     :: C f [LabelEPC]
  , _bizTransactionID           :: C f Int32 -- probably link to a table of biztransactions
  , _transformationID           :: PrimaryKey TransformationT f }
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
  primaryKey = WhatId . _whatID

data WhyT f = Why
  { _whyID                      :: C f (Auto Int32)
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
  primaryKey = WhyId . _whyID

data WhereT f = Where
  { _whereID                    :: C f (Auto Int32)
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
  primaryKey = WhereId . _whereID


data WhenT f = When
  { _whenID                      :: C f (Auto Int32)
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
  primaryKey = WhenId . _whenID

data LabelEventT f = LabelEvent
  { _labelEventID               :: C f (Auto Int32)
  , _labelEventLabelID          :: PrimaryKey LabelT f
  , _labelEventEventID          :: PrimaryKey EventT f }
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
  primaryKey = LabelEventId . _labelEventID


data SupplyChainDb f = SupplyChainDb
  { _supplyChainUsers           :: f (TableEntity UserT)
  , _supplyChainKey            :: f (TableEntity KeyT)
  , _supplyChainBusiness      :: f (TableEntity BusinessT)
  , _supplyChainContact        :: f (TableEntity ContactsT)
  , _supplyChainLabel          :: f (TableEntity LabelT)
  , _supplyChainTransformation :: f (TableEntity TransformationT)
  , _supplyChainLocation       :: f (TableEntity LocationT)
  , _supplyChainEvent          :: f (TableEntity EventT)
  , _supplyChainWhat            :: f (TableEntity WhatT)
  , _supplyChainWhy             :: f (TableEntity WhyT)
  , _supplyChainWhere           :: f (TableEntity WhereT)
  , _supplyChainWhen            :: f (TableEntity WhenT)
  , _supplyChainLabelEvent     :: f (TableEntity LabelEventT) }
  deriving Generic
instance Database SupplyChainDb

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EventType where
  sqlValueSyntax = autoSqlValueSyntax

instance FromField EventType where
  fromField f mdata = do 
                        x <- readMaybe <$> fromField f mdata
                        case x of
                          Nothing -> returnError ConversionFailed f "Could not 'read' value for 'EventType'"
                          Just x -> pure x

instance FromBackendRow Postgres EventType

supplyChainDb :: DatabaseSettings be SupplyChainDb
supplyChainDb = defaultDbSettings
