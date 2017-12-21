{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module StorageBeam where

import Database.Beam
import Database.Beam.Sqlite

import Data.Text (Text)


data UserT f = UserT
  { _userID              :: Columnar f (Auto Int)
  , _bizID               :: Columnar f PrimaryKey BusinessT f
  , _firstName           :: Columnar f Text
  , _lastName            :: Columnar f Text
  , _phoneNumber         :: Columnar f Text
  , _passwordHash        :: Columnar f Text --XXX - should this be blob?
  , _emailAddress        :: Columnar f Text
  }
  deriving Generic

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f (Auto Int))
    deriving Generic
  primaryKey = UserId . _userId

instance Beamable UserT
instance Beamable (PrimaryKey UserId)

type User = UserT Identity
  deriving instance Show User



data KeysT f = KeysT
  { _keyID              :: Columnar f (Auto Int32)
  , _userID             :: Columnar f PrimaryKey UsersT f
  , _rsa_n              :: Columnar f Int32 --XXX should this be Int64?
  , _rsa_e              :: Columnar f Int32 -- as above
  , _creationTime       :: Columnar f Int32 --XXX date.. Int64?
  , _revocationTime     :: Columnar f Int32 -- as above
  }
  deriving Generic
instance Beamable KeysT

data BusinessT f = BusinessT
  { _bizID             :: Columnar f (Auto Int32)
  , _bizName           :: Columnar f Text
  , _gs1CompanyPrefix  :: Columnar f Int32
  , _businessFunction  :: Columnar f Text
  , _siteName          :: Columnar f Text
  , _bizAddress        :: Columnar f Text
  , _lat               :: Columnar f Float
  , _long              :: Columnar f Float
  }
  deriving Generic
instance Beamable BusinessT

data ContactsT f = ContactsT
  { _contactID         :: Columnar f Int32
  , _user1ID           :: Columnar f PrimaryKey UsersT f
  , _user2ID           :: Columnar f PrimaryKey UsersT f
  }
  deriving Generic
instance Beamable ContactsT


data LabelsT f = LabelsT
  { _labelId           :: Columnar f Int32
  , _gs1CompanyPrefix  :: Columnar f Text --should this be bizID instead?
  , _itemReference     :: Columnar f Text
  , _serialNumber      :: Columnar f Text
  , _state             :: Columnar f Text
  , _labelType         :: Columnar f Text
  , _lot               :: Columnar f Text
  }
  deriving Generic
instance Beamable LabelsT

data ItemT f = ItemT
  { _itemId            :: Columnar f Int32
  , _labelId           :: Columnar f PriamryKey LabelsT f
  , _itemDescription   :: Columnar f Text
  }
  deriving Generic
instance Beamable ItemT


data TransformationT = TransformationT
  { _transformationId             :: Columnar f Int32
  , _transformationDescription    :: Columnar f Text
  , _transformBizID               :: Columnar f PrimaryKey BusinessT f
  }
  deriving Generic
instance Beamable TransformationT

data LocationT = LocationT
  { _locationID                  :: Columnar f Int32
  , _locationBizID               :: Columnar f PrimaryKey BusinessT f
  , _lat                         :: Columnar f Float
  , _long                        :: Columnar f Float
  }
  deriving Generic
instance Beamable LocationT

data EventsT = EventsT
  { _eventID                      :: Columnar f Int32
  , _labelID                      :: Columnar f PrimaryKey BusinessT f --the label scanned to generate this event.
  , _whatID                       :: Columnar f Int32
  , _whyID                        :: Columnar f Int32
  , _whereID                      :: Columnar f Int32
  , _whenID                       :: Columnar f Int32
  , _createdBy                    :: Columnar f Int32 --userID
  , _jsonEvent                    :: Columnar f Text
  }
  deriving Generic
instance Beamable EventsT

data EventType = ObjectEvent | AggregationEvent | TransactionEvent | TransformationEvent
  deriving (Show, Enum, Read)

data WhatT = WhatT
  { _whatID                       :: Columnar f Int32
  , _whatType                     :: Columnar f EventType
  , _action                       :: Columnar f Action
  , _parent                       :: Columnar f Int32 --labelID
  , _input                        :: Columnar f [LabelEPC]
  , _output                       :: Columnar f [LabelEPC]
  , _bizTransactionID             :: Columnar f Int32 -- probably link to a table of biztransactions
  , _transformationID             :: Columnar f PrimaryKey TransformationT f
    }
    deriving Generic
instance Beamable WhatT


data WhyT = WhyT
  { _whyID                      :: Columnar f Int32
  , _bizStep                    :: Columnar f BizStep
  , _disposition                :: Columnar f Disposition
  }
  deriving Generic
instance Beamable WhyT

data WhereT = WhereT
  { _whereID                   :: Columnar f Int32
  , _readPoint                 :: Columnar f PrimaryKey LocationT f
  , _bizLocation               :: Columnar f PrimaryKey LocationT f
  , _srcType                   :: Columnar f SourceDestType
  , _destType                  :: Columnar f SourceDestType
  }
  deriving Generic
instance Beamable WhereT

data WhenT = WhenT
 { _whenID                   :: Columnar f Int32
 , _eventTime                :: Columnar f Int64
 , _recordTime               :: Columnar f Int64
 , _timeZone                 :: Columnar f TimeZone
 }
 deriving Generic
instance Beamable WhenT


data LabelEventsT = LabelEventsT
 { _labelID                 :: Columnar f PrimaryKey LabelsT f
 , _eventID                 :: Columnar f PrimaryKey LocationT f
 }


data SupplyChainDb f = SupplyChainDb
  {
    _supplyChainUsers           :: (TableEntity UserT)
    _supplyChainKeys            :: (TableEntity KeysT)
    _supplyChainBusinesses      :: (TableEntity BusinessT)
    _supplyChainContacts        :: (TableEntity ContactsT)
    _supplyChainLabels          :: (TableEntity LabelsT)
    _supplyChainTransformations :: (TableEntity TransformationT)
    _supplyChainLocations       :: (TableEntity LocationT)
    _supplyChainEvents          :: (TableEntity EventsT)
    _supplyChainWhat            :: (TableEntity WhatT)
    _supplyChainWhy             :: (TableEntity WhyT)
    _supplyChainWhere           :: (TableEntity WhereT)
    _supplyChainWhen            :: (TableEntity WhenT)
    _supplyChainLabelEvents     :: (TableEntity LabelEventsT)
  }
  deriving Generic
instance Database SupplyChainDb


