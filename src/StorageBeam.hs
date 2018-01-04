{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StorageBeam where

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)


data UserT f = User
  { _userID              :: Columnar f (Auto Int)
  , _bizID               :: Columnar f PrimaryKey BusinessT f
  , _firstName           :: Columnar f Text
  , _lastName            :: Columnar f Text
  , _phoneNumber         :: Columnar f Text
  , _passwordHash        :: Columnar f Text --XXX - should this be blob?
  , _emailAddress        :: Columnar f Text }
  deriving Generic

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (Auto Int))
    deriving Generic
  primaryKey = UserId . _userId

instance Beamable UserT
instance Beamable (PrimaryKey UserId)

type User = UserT Identity
deriving instance Show User

data KeysT f = Keys
  { _keyID              :: Columnar f (Auto Int32)
  , _userID             :: Columnar f PrimaryKey UsersT f
  , _rsa_n              :: Columnar f Int32 --XXX should this be Int64?
  , _rsa_e              :: Columnar f Int32 -- as above
  , _creationTime       :: Columnar f Int32 --XXX date.. Int64?
  , _revocationTime     :: Columnar f Int32 -- as above
  }
  deriving Generic
instance Beamable KeysT

data BusinessT f = Business
  { _bizID             :: Columnar f (Auto Int32)
  , _bizName           :: Columnar f Text
  , _gs1CompanyPrefix  :: Columnar f Int32
  , _businessFunction  :: Columnar f Text
  , _siteName          :: Columnar f Text
  , _bizAddress        :: Columnar f Text
  , _lat               :: Columnar f Float
  , _long              :: Columnar f Float }
  deriving Generic
instance Beamable BusinessT

data ContactsT f = Contacts
  { _contactID         :: Columnar f Int32
  , _user1ID           :: Columnar f PrimaryKey UsersT f
  , _user2ID           :: Columnar f PrimaryKey UsersT f }
  deriving Generic
instance Beamable ContactsT


data LabelsT f = Labels
  { _labelId           :: Columnar f (Auto Int32)
  , _gs1CompanyPrefix  :: Columnar f Text --should this be bizID instead?
  , _itemReference     :: Columnar f Text
  , _serialNumber      :: Columnar f Text
  , _state             :: Columnar f Text
  , _labelType         :: Columnar f Text
  , _lot               :: Columnar f Text }
  deriving Generic
instance Beamable LabelsT

data ItemT f = Item
  { _itemId            :: Columnar f Int32
  , _labelId           :: Columnar f PriamryKey LabelsT f
  , _itemDescription   :: Columnar f Text }
  deriving Generic
instance Beamable ItemT


data TransformationT = Transformation
  { _transformationId             :: Columnar f (Auto Int32)
  , _transformationDescription    :: Columnar f Text
  , _transformBizID               :: Columnar f PrimaryKey BusinessT f }
  deriving Generic
instance Beamable TransformationT

data LocationT = Location
  { _locationID                  :: Columnar f Int32
  , _locationBizID               :: Columnar f PrimaryKey BusinessT f
  , _lat                         :: Columnar f Float
  , _long                        :: Columnar f Float }
  deriving Generic
instance Beamable LocationT

data EventsT = Events
  { _eventID                      :: Columnar f (Auto Int32)
  , _foreignEventID               :: Columnar f Text
  , _labelID                      :: Columnar f PrimaryKey BusinessT f --the label scanned to generate this event.
  , _whatID                       :: Columnar f PrimaryKey WhatT f
  , _whyID                        :: Columnar f PrimaryKey WhyT f
  , _whereID                      :: Columnar f PrimaryKey WhereT f
  , _whenID                       :: Columnar f PrimaryKey WhenT f
  , _createdBy                    :: Columnar f PrimaryKey UserT f
  , _jsonEvent                    :: Columnar f Text }
  deriving Generic
instance Beamable EventsT

data EventType = ObjectEvent
               | AggregationEvent
               | TransactionEvent
               | TransformationEvent
                 deriving (Show, Enum, Read)

data WhatT = What
  { _whatID                       :: Columnar f (Auto Int32)
  , _whatType                     :: Columnar f EventType
  , _action                       :: Columnar f Action
  , _parent                       :: Columnar f PrimaryKey LabelT f
  , _input                        :: Columnar f [LabelEPC]
  , _output                       :: Columnar f [LabelEPC]
  , _bizTransactionID             :: Columnar f Int32 -- probably link to a table of biztransactions
  , _transformationID             :: Columnar f PrimaryKey TransformationT f }
  deriving Generic
instance Beamable WhatT


data WhyT = Why
  { _whyID                      :: Columnar f (Auto Int32)
  , _bizStep                    :: Columnar f BizStep
  , _disposition                :: Columnar f Disposition }
  deriving Generic
instance Beamable WhyT

data WhereT = Where
  { _whereID                   :: Columnar f (Auto Int32)
  , _readPoint                 :: Columnar f PrimaryKey LocationT f
  , _bizLocation               :: Columnar f PrimaryKey LocationT f
  , _srcType                   :: Columnar f SourceDestType
  , _destType                  :: Columnar f SourceDestType }
  deriving Generic
instance Beamable WhereT

data WhenT = When
 { _whenID                   :: Columnar f Int32
 , _eventTime                :: Columnar f Int64
 , _recordTime               :: Columnar f Int64
 , _timeZone                 :: Columnar f TimeZone }
 deriving Generic
instance Beamable WhenT


data LabelEventsT = LabelEvents
 { _labelID                 :: Columnar f PrimaryKey LabelsT f
 , _eventID                 :: Columnar f PrimaryKey LocationT f }


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
