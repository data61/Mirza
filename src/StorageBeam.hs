{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StorageBeam where

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)


data UserT f = User
  { _userID              :: C f (Auto Int)
  , _bizID               :: C f PrimaryKey BusinessT f
  , _firstName           :: C f Text
  , _lastName            :: C f Text
  , _phoneNumber         :: C f Text
  , _passwordHash        :: C f Text --XXX - should this be blob?
  , _emailAddress        :: C f Text }
  deriving Generic

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f (Auto Int))
    deriving Generic
  primaryKey = UserId . _userId

instance Beamable UserT
instance Beamable (PrimaryKey UserId)

type User = UserT Identity
deriving instance Show User

data KeysT f = Keys
  { _keyID              :: C f (Auto Int32)
  , _userID             :: C f PrimaryKey UsersT f
  , _rsa_n              :: C f Int32 --XXX should this be Int64?
  , _rsa_e              :: C f Int32 -- as above
  , _creationTime       :: C f Int32 --XXX date.. Int64?
  , _revocationTime     :: C f Int32 -- as above
  }
  deriving Generic
instance Beamable KeysT

data BusinessT f = Business
  { _bizID             :: C f (Auto Int32)
  , _bizName           :: C f Text
  , _gs1CompanyPrefix  :: C f Int32
  , _businessFunction  :: C f Text
  , _siteName          :: C f Text
  , _bizAddress        :: C f Text
  , _lat               :: C f Float
  , _long              :: C f Float }
  deriving Generic
instance Beamable BusinessT

data ContactsT f = Contacts
  { _contactID         :: C f Int32
  , _user1ID           :: C f PrimaryKey UsersT f
  , _user2ID           :: C f PrimaryKey UsersT f }
  deriving Generic
instance Beamable ContactsT


data LabelsT f = Labels
  { _labelId           :: C f (Auto Int32)
  , _gs1CompanyPrefix  :: C f Text --should this be bizID instead?
  , _itemReference     :: C f Text
  , _serialNumber      :: C f Text
  , _state             :: C f Text
  , _labelType         :: C f Text
  , _lot               :: C f Text }
  deriving Generic
instance Beamable LabelsT

data ItemT f = Item
  { _itemId            :: C f Int32
  , _labelId           :: C f PriamryKey LabelsT f
  , _itemDescription   :: C f Text }
  deriving Generic
instance Beamable ItemT


data TransformationT = Transformation
  { _transformationId             :: C f (Auto Int32)
  , _transformationDescription    :: C f Text
  , _transformBizID               :: C f PrimaryKey BusinessT f }
  deriving Generic
instance Beamable TransformationT

data LocationT = Location
  { _locationID                  :: C f Int32
  , _locationBizID               :: C f PrimaryKey BusinessT f
  , _lat                         :: C f Float
  , _long                        :: C f Float }
  deriving Generic
instance Beamable LocationT

data EventsT = Events
  { _eventID                      :: C f (Auto Int32)
  , _foreignEventID               :: C f Text
  , _labelID                      :: C f PrimaryKey BusinessT f --the label scanned to generate this event.
  , _whatID                       :: C f PrimaryKey WhatT f
  , _whyID                        :: C f PrimaryKey WhyT f
  , _whereID                      :: C f PrimaryKey WhereT f
  , _whenID                       :: C f PrimaryKey WhenT f
  , _createdBy                    :: C f PrimaryKey UserT f
  , _jsonEvent                    :: C f Text }
  deriving Generic
instance Beamable EventsT

data EventType = ObjectEvent
               | AggregationEvent
               | TransactionEvent
               | TransformationEvent
                 deriving (Show, Enum, Read)

data WhatT = What
  { _whatID                       :: C f (Auto Int32)
  , _whatType                     :: C f EventType
  , _action                       :: C f Action
  , _parent                       :: C f PrimaryKey LabelT f
  , _input                        :: C f [LabelEPC]
  , _output                       :: C f [LabelEPC]
  , _bizTransactionID             :: C f Int32 -- probably link to a table of biztransactions
  , _transformationID             :: C f PrimaryKey TransformationT f }
  deriving Generic
instance Beamable WhatT


data WhyT = Why
  { _whyID                      :: C f (Auto Int32)
  , _bizStep                    :: C f BizStep
  , _disposition                :: C f Disposition }
  deriving Generic
instance Beamable WhyT

data WhereT = Where
  { _whereID                   :: C f (Auto Int32)
  , _readPoint                 :: C f PrimaryKey LocationT f
  , _bizLocation               :: C f PrimaryKey LocationT f
  , _srcType                   :: C f SourceDestType
  , _destType                  :: C f SourceDestType }
  deriving Generic
instance Beamable WhereT

data WhenT = When
 { _whenID                   :: C f Int32
 , _eventTime                :: C f Int64
 , _recordTime               :: C f Int64
 , _timeZone                 :: C f TimeZone }
 deriving Generic
instance Beamable WhenT


data LabelEventsT = LabelEvents
 { _labelID                 :: C f PrimaryKey LabelsT f
 , _eventID                 :: C f PrimaryKey LocationT f }


data SupplyChainDb f = SupplyChainDb
  { _supplyChainUsers           :: f (TableEntity UserT)
  , _supplyChainKeys            :: f (TableEntity KeysT)
  , _supplyChainBusinesses      :: f (TableEntity BusinessT)
  , _supplyChainContacts        :: f (TableEntity ContactsT)
  , _supplyChainLabels          :: f (TableEntity LabelsT)
  , _supplyChainTransformations :: f (TableEntity TransformationT)
  , _supplyChainLocations       :: f (TableEntity LocationT)
  , _supplyChainEvents          :: f (TableEntity EventsT)
  , _supplyChainWhat            :: f (TableEntity WhatT)
  , _supplyChainWhy             :: f (TableEntity WhyT)
  , _supplyChainWhere           :: f (TableEntity WhereT)
  , _supplyChainWhen            :: f (TableEntity WhenT)
  , _supplyChainLabelEvents     :: f (TableEntity LabelEventsT) }
  deriving Generic
instance Database SupplyChainDb
