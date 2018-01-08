{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DataKinds          #-}

module StorageBeam where

import Database.Beam as B
import Database.Beam.Postgres
import Database.PostgreSQL.Simple

import Data.GS1.EventID
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy

import Data.Text (Text)
import Data.Int
import Data.Time

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

data KeysT f = Keys
  { _keyID              :: C f (Auto Int32)
  , _keysUserID         :: PrimaryKey UserT f
  , _rsa_n              :: C f Int32 --XXX should this be Int64?
  , _rsa_e              :: C f Int32 -- as above
  , _creationTime       :: C f Int32 --XXX date.. Int64?
  , _revocationTime     :: C f Int32 -- as above
  }
  deriving Generic
type Keys = KeysT Identity
type KeyId = PrimaryKey KeysT Identity

deriving instance Show Keys

instance Beamable KeysT
instance Beamable (PrimaryKey KeysT)

instance Table KeysT where
  data PrimaryKey KeysT f = KeyId (C f (Auto Int32))
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

data LabelsT f = Labels
  { _labelID                 :: C f (Auto Int32)
  , _labelsGs1CompanyPrefix  :: C f Text --should this be bizID instead?
  , _itemReference           :: C f Text
  , _serialNumber            :: C f Text
  , _state                   :: C f Text
  , _labelType               :: C f Text
  , _lot                     :: C f Text }
  deriving Generic
type Labels = LabelsT Identity
type LabelId = PrimaryKey LabelsT Identity

deriving instance Show Labels

instance Beamable LabelsT
instance Beamable (PrimaryKey LabelsT)
deriving instance Show (PrimaryKey LabelsT Identity)

instance Table LabelsT where
  data PrimaryKey LabelsT f = LabelId (C f (Auto Int32))
    deriving Generic
  primaryKey = LabelId . _labelID

data ItemT f = Item
  { _itemId            :: C f Int32
  , _itemLabelId       :: PrimaryKey LabelsT f
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
  { _locationID                 :: C f Int32
  , _locationBizID              :: PrimaryKey BusinessT f
  , _locationLat                :: C f Float
  , _locationLong               :: C f Float }
  deriving Generic
instance Beamable LocationT
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

data EventsT f = Events
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
type Events = EventsT Identity
type EventsId = PrimaryKey EventsT Identity

deriving instance Show Events

instance Beamable EventsT
instance Beamable (PrimaryKey EventsT)
deriving instance Show (PrimaryKey EventsT Identity)

instance Table EventsT where
  data PrimaryKey EventsT f = EventsId (C f (Auto Int32))
    deriving Generic
  primaryKey = EventsId . _eventID

data EventType = ObjectEvent
               | AggregationEvent
               | TransactionEvent
               | TransformationEvent
                 deriving (Show, Enum, Read)

data WhatT f = What
  { _whatID                     :: C f (Auto Int32)
  , _whatType                   :: C f EventType
  , _action                     :: C f Action
  , _parent                     :: PrimaryKey LabelsT f
  , _input                      :: C f [LabelEPC]
  , _output                     :: C f [LabelEPC]
  , _bizTransactionID           :: C f Int32 -- probably link to a table of biztransactions
  , _transformationID           :: PrimaryKey TransformationT f }
  deriving Generic
instance Beamable WhatT


data WhyT f = Why
  { _whyID                      :: C f (Auto Int32)
  , _bizStep                    :: C f BizStep
  , _disposition                :: C f Disposition }
  deriving Generic
instance Beamable WhyT

data WhereT f = Where
  { _whereID                    :: C f (Auto Int32)
  , _readPoint                  :: PrimaryKey LocationT f
  , _bizLocation                :: PrimaryKey LocationT f
  , _srcType                    :: C f SourceDestType
  , _destType                   :: C f SourceDestType }
  deriving Generic
instance Beamable WhereT

data WhenT f = When
 { _whenID                      :: C f (Auto Int32)
 , _eventTime                   :: C f Int64
 , _recordTime                  :: C f Int64
 , _timeZone                    :: C f TimeZone }
 deriving Generic
instance Beamable WhenT


-- doesn't have a primary key
data LabelEventsT f = LabelEvents
 { _labelEventsID               :: C f (Auto Int32)
 , _labelEventsLabelID          :: PrimaryKey LabelsT f
 , _labelEventsEventID          :: PrimaryKey EventsT f }


data SupplyChainDb f = SupplyChainDb
  { _supplyChainUsers           :: C f (TableEntity UserT)
  , _supplyChainKeys            :: C f (TableEntity KeysT)
  , _supplyChainBusinesses      :: C f (TableEntity BusinessT)
  , _supplyChainContacts        :: C f (TableEntity ContactsT)
  , _supplyChainLabels          :: C f (TableEntity LabelsT)
  , _supplyChainTransformations :: C f (TableEntity TransformationT)
  , _supplyChainLocations       :: C f (TableEntity LocationT)
  , _supplyChainEvents          :: C f (TableEntity EventsT)
  , _supplyChainWhat            :: C f (TableEntity WhatT)
  , _supplyChainWhy             :: C f (TableEntity WhyT)
  , _supplyChainWhere           :: C f (TableEntity WhereT)
  , _supplyChainWhen            :: C f (TableEntity WhenT)
  , _supplyChainLabelEvents     :: C f (TableEntity LabelEventsT) }
  deriving Generic
instance Database SupplyChainDb
