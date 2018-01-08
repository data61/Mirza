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
instance Beamable KeysT

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
instance Beamable BusinessT

data ContactsT f = Contacts
  { _contactID                :: C f Int32
  , _contactUser1ID           :: PrimaryKey UserT f
  , _contactUser2ID           :: PrimaryKey UserT f }
  deriving Generic
instance Beamable ContactsT


data LabelsT f = Labels
  { _labelId                 :: C f (Auto Int32)
  , _labelsGs1CompanyPrefix  :: C f Text --should this be bizID instead?
  , _itemReference           :: C f Text
  , _serialNumber            :: C f Text
  , _state                   :: C f Text
  , _labelType               :: C f Text
  , _lot                     :: C f Text }
  deriving Generic
instance Beamable LabelsT

data ItemT f = Item
  { _itemId            :: C f Int32
  , _itemLabelId       :: PrimaryKey LabelsT f
  , _itemDescription   :: C f Text }
  deriving Generic
instance Beamable ItemT


data TransformationT f = Transformation
  { _transformationId           :: C f (Auto Int32)
  , _transformationDescription  :: C f Text
  , _transformBizID             :: PrimaryKey BusinessT f }
  deriving Generic
instance Beamable TransformationT

data LocationT f = Location
  { _locationID                 :: C f Int32
  , _locationBizID              :: PrimaryKey BusinessT f
  , _locationLat                :: C f Float
  , _long                       :: C f Float }
  deriving Generic
instance Beamable LocationT

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
instance Beamable EventsT

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
 { _whenID                      :: C f Int32
 , _eventTime                   :: C f Int64
 , _recordTime                  :: C f Int64
 , _timeZone                    :: C f TimeZone }
 deriving Generic
instance Beamable WhenT


data LabelEventsT f = LabelEvents
 { _labelID                     :: PrimaryKey LabelsT f
 , _labelEventID                :: PrimaryKey LocationT f }


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
