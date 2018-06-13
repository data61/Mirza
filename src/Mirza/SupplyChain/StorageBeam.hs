-- Please do not remove any commented out code in this module.
-- We are working with a WIP library, namely Beam.
-- In table definitions
-- most of the commented out code are there for
-- 1. Reference. To see how the Haskell data type looks like
-- 2. As a record of what the types were previously
--  --> to help us decide whether or not to roll back some changes

-- If you see the word ``WAITING_FOR_LIB_FIX`` anywhere, it probably means that
-- we are waiting on a library (eg. Beam) to implement something. For example,
-- migration support for UTCTime

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
-- The migration script has been moved to the module MigrateScript
-- If some definition is changed here, please make the equivalent change
-- in MigrateScript
module Mirza.SupplyChain.StorageBeam where

import qualified Mirza.SupplyChain.MigrateUtils as MU

import qualified Data.GS1.EPC                   as EPC
import qualified Data.GS1.Event                 as Ev

import           Control.Lens
import           Data.Aeson                     (FromJSON, ToJSON)
import           Data.ByteString                (ByteString)
import           Data.Swagger                   (ToSchema)
import           Data.Text                      (Text)
import           Data.Time
import           Data.UUID                      (UUID)
import           Database.Beam                  as B
import           Database.Beam.Postgres

type PrimaryKeyType = UUID
-- IMPLEMENTME - NOT NOW
-- Change PrimaryKeyType to ``Auto Int`` and define the instances below
-- instance ToSchema PrimaryKeyType
-- instance ToParamSchema PrimaryKeyType where
--   -- TODO = refactor this, want toParamSchema for ToParamSchema UUID
--   -- https://github.com/GetShopTV/swagger2/blob/master/src/Data/Swagger/Internal/ParamSchema.hs#L268
--   toParamSchema _ = mempty & type_ .~ SwaggerString & Data.Swagger.format ?~ "uuid"
-- instance FromHttpApiData PrimaryKeyType where
--   -- parseUrlPiece :: Text -> Either Text PrimaryKeyType
--   parseUrlPiece t = error $ show t ++ " parseUP"
--   parseQueryParam t = error $ show t ++ " parseQP"

data UserT f = User
  { user_id       :: C f PrimaryKeyType
  , user_biz_id   :: PrimaryKey BusinessT f
  , first_name    :: C f Text
  , last_name     :: C f Text
  , phone_number  :: C f Text
  , password_hash :: C f ByteString --XXX - should this be blob?
  , email_address :: C f Text }
  deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Show (PrimaryKey UserT Identity)

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = UserId . user_id
deriving instance Eq (PrimaryKey UserT Identity)

data KeyT f = Key
  { key_id          :: C f PrimaryKeyType
  , key_user_id     :: PrimaryKey UserT f
  , pem_str         :: C f Text
  , creation_time   :: C f LocalTime -- UTCTime
  , revocation_time :: C f (Maybe LocalTime) -- UTCTime
  }
  deriving Generic
type Key = KeyT Identity
type KeyId = PrimaryKey KeyT Identity

deriving instance Show Key

instance Beamable KeyT
instance Beamable (PrimaryKey KeyT)

instance Table KeyT where
  data PrimaryKey KeyT f = KeyId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = KeyId . key_id
deriving instance Eq (PrimaryKey KeyT Identity)

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 11

data BusinessT f = Business
  { biz_gs1_company_prefix :: C f EPC.GS1CompanyPrefix -- PrimaryKey
  , biz_name               :: C f Text
  , biz_function           :: C f Text
  , biz_site_name          :: C f Text
  , biz_address            :: C f Text
  , biz_lat                :: C f Double
  , biz_long               :: C f Double }
  deriving Generic
type Business = BusinessT Identity
type BizId = PrimaryKey BusinessT Identity

deriving instance Show Business

instance Beamable BusinessT
instance Beamable (PrimaryKey BusinessT)
deriving instance Show (PrimaryKey BusinessT Identity)

instance Table BusinessT where
  data PrimaryKey BusinessT f = BizId (C f EPC.GS1CompanyPrefix)
    deriving Generic
  primaryKey = BizId . biz_gs1_company_prefix
deriving instance Eq (PrimaryKey BusinessT Identity)

data ContactT f = Contact
  { contact_id       :: C f PrimaryKeyType
  , contact_user1_id :: PrimaryKey UserT f
  , contact_user2_id :: PrimaryKey UserT f }
  deriving Generic

type Contact = ContactT Identity
type ContactId = PrimaryKey ContactT Identity

deriving instance Show Contact

instance Beamable ContactT
instance Beamable (PrimaryKey ContactT)
deriving instance Show (PrimaryKey ContactT Identity)

instance Table ContactT where
  data PrimaryKey ContactT f = ContactId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = ContactId . contact_id

data LabelT f = Label
  { label_id                 :: C f PrimaryKeyType
  , label_type               :: C f (Maybe MU.LabelType)
  , label_what_id            :: PrimaryKey WhatT f
  , label_gs1_company_prefix :: C f EPC.GS1CompanyPrefix --should this be bizId instead?
  , item_reference           :: C f (Maybe EPC.ItemReference)
  , serial_number            :: C f (Maybe EPC.SerialNumber)
  , state                    :: C f (Maybe Text)
  , lot                      :: C f (Maybe EPC.Lot)
  , sgtin_filter_value       :: C f (Maybe EPC.SGTINFilterValue)
  , asset_type               :: C f (Maybe EPC.AssetType)
  , quantity_amount          :: C f (Maybe EPC.Amount)
  , quantity_uom             :: C f (Maybe EPC.Uom)
  }
  deriving Generic
type Label = LabelT Identity
type LabelId = PrimaryKey LabelT Identity

deriving instance Show Label

instance Beamable LabelT
instance Beamable (PrimaryKey LabelT)
deriving instance Show (PrimaryKey LabelT (Nullable Identity))
deriving instance Show (PrimaryKey LabelT Identity)

instance Table LabelT where
  data PrimaryKey LabelT f = LabelId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = LabelId . label_id
deriving instance Eq (PrimaryKey LabelT Identity)

data WhatLabelT f = WhatLabel
  { what_label_id       :: C f PrimaryKeyType
  , what_label_what_id  :: PrimaryKey WhatT f
  , what_label_label_id :: PrimaryKey LabelT f }
  deriving Generic

type WhatLabel = WhatLabelT Identity
type WhatLabelId = PrimaryKey WhatLabelT Identity

deriving instance Show WhatLabel

instance Beamable WhatLabelT
instance Beamable (PrimaryKey WhatLabelT)
deriving instance Show (PrimaryKey WhatLabelT Identity)

instance Table WhatLabelT where
  data PrimaryKey WhatLabelT f = WhatLabelId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhatLabelId . what_label_id


data ItemT f = Item
  { item_id          :: C f PrimaryKeyType
  , item_label_id    :: PrimaryKey LabelT f
  , item_description :: C f Text }
  deriving Generic
type Item = ItemT Identity
type ItemId = PrimaryKey ItemT Identity

deriving instance Show Item

instance Beamable ItemT
instance Beamable (PrimaryKey ItemT)
deriving instance Show (PrimaryKey ItemT Identity)

instance Table ItemT where
  data PrimaryKey ItemT f = ItemId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = ItemId . item_id

data TransformationT f = Transformation
  { transformation_id          :: C f PrimaryKeyType
  , transformation_description :: C f Text
  , transformation_biz_id      :: PrimaryKey BusinessT f }
  deriving Generic
type Transformation = TransformationT Identity
type TransformationId = PrimaryKey TransformationT Identity

deriving instance Show Transformation

instance Beamable TransformationT
instance Beamable (PrimaryKey TransformationT)
deriving instance Show (PrimaryKey TransformationT Identity)
deriving instance Show (PrimaryKey TransformationT (Nullable Identity))

instance Table TransformationT where
  data PrimaryKey TransformationT f = TransformationId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = TransformationId . transformation_id
deriving instance Eq (PrimaryKey TransformationT Identity)

data LocationT f = Location
  { location_id     :: C f EPC.LocationReference
  , location_biz_id :: PrimaryKey BusinessT f
  -- this needs to be locationReferenceNum
  , location_lat    :: C f Double
  , location_long   :: C f Double }
  deriving Generic

type Location = LocationT Identity
type LocationId = PrimaryKey LocationT Identity

deriving instance Show Location

instance Beamable LocationT
instance Beamable (PrimaryKey LocationT)
deriving instance Show (PrimaryKey LocationT Identity)

instance Table LocationT where
  data PrimaryKey LocationT f = LocationId (C f EPC.LocationReference)
    deriving Generic
  primaryKey = LocationId . location_id

data EventT f = Event
  { event_id         :: C f PrimaryKeyType
  , foreign_event_id :: C f (Maybe PrimaryKeyType) -- Event ID from XML from foreign systems.
  , event_created_by :: PrimaryKey UserT f
  , json_event       :: C f Text }
  deriving Generic
type Event = EventT Identity
type EventId = PrimaryKey EventT Identity

instance ToSchema EventId
instance FromJSON EventId
instance ToJSON EventId

deriving instance Show Event

instance Beamable EventT
instance Beamable (PrimaryKey EventT)
deriving instance Show (PrimaryKey EventT Identity)

-- | Utility function to extract ``eventId``
unEventId :: PrimaryKey EventT f -> C f PrimaryKeyType
unEventId (EventId eventId) = eventId

instance Table EventT where
  data PrimaryKey EventT f = EventId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = EventId . event_id
deriving instance Eq (PrimaryKey EventT Identity)

data WhatT f = What
  { what_id                 :: C f PrimaryKeyType
  , what_event_type         :: C f (Maybe Ev.EventType)
  , action                  :: C f (Maybe EPC.Action)
  , parent                  :: PrimaryKey LabelT (Nullable f)
  , what_biz_transaction_id :: PrimaryKey BizTransactionT (Nullable f)
  , what_transformation_id  :: PrimaryKey TransformationT (Nullable f)
  , what_event_id           :: PrimaryKey EventT f }
  deriving Generic

type What = WhatT Identity
type WhatId = PrimaryKey WhatT Identity

deriving instance Show What
instance Beamable WhatT

instance Beamable (PrimaryKey WhatT)
deriving instance Show (PrimaryKey WhatT Identity)

instance Table WhatT where
  data PrimaryKey WhatT f = WhatId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhatId . what_id
deriving instance Eq (PrimaryKey WhatT Identity)


data BizTransactionT f = BizTransaction
  { biz_transaction_id       :: C f PrimaryKeyType
  , biz_transaction_type_id  :: C f Text
  , biz_transaction_id_urn   :: C f Text
  , biz_transaction_event_id :: PrimaryKey EventT f }

  deriving Generic

type BizTransaction = BizTransactionT Identity
type BizTransactionId = PrimaryKey BizTransactionT Identity

deriving instance Show BizTransaction
instance Beamable BizTransactionT

instance Beamable (PrimaryKey BizTransactionT)
deriving instance Show (PrimaryKey BizTransactionT Identity)
deriving instance Show (PrimaryKey BizTransactionT (Nullable Identity))

instance Table BizTransactionT where
  data PrimaryKey BizTransactionT f = BizTransactionId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = BizTransactionId . biz_transaction_id
deriving instance Eq (PrimaryKey BizTransactionT Identity)

data WhyT f = Why
  { why_id       :: C f PrimaryKeyType
  , biz_step     :: C f (Maybe Text) -- EPC.BizStep
  , disposition  :: C f (Maybe Text) -- EPC.Disposition
  , why_event_id :: PrimaryKey EventT f }

  deriving Generic

type Why = WhyT Identity
type WhyId = PrimaryKey WhyT Identity
deriving instance Show Why
instance Beamable WhyT
instance Beamable (PrimaryKey WhyT)
deriving instance Show (PrimaryKey WhyT Identity)

instance Table WhyT where
  data PrimaryKey WhyT f = WhyId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhyId . why_id

data WhereT f = Where
  { where_id                 :: C f PrimaryKeyType
  , where_gs1_company_prefix :: C f EPC.GS1CompanyPrefix
  , where_source_dest_type   :: C f (Maybe EPC.SourceDestType)
  , where_gs1_location_id    :: C f EPC.LocationReference
  , where_location_field     :: C f MU.LocationField
  , where_sgln_ext           :: C f (Maybe EPC.SGLNExtension)
  , where_event_id           :: PrimaryKey EventT f }
  deriving Generic

type Where = WhereT Identity
type WhereId = PrimaryKey WhereT Identity
deriving instance Show Where
instance Beamable WhereT
instance Beamable (PrimaryKey WhereT)
deriving instance Show (PrimaryKey WhereT Identity)

instance Table WhereT where
  data PrimaryKey WhereT f = WhereId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhereId . where_id

type TzOffsetString = Text

data WhenT f = When
  { when_id       :: C f PrimaryKeyType
  , event_time    :: C f LocalTime
  , record_time   :: C f (Maybe LocalTime)
  , time_zone     :: C f TzOffsetString -- TimeZone
  , when_event_id :: PrimaryKey EventT f }
  deriving Generic

type When = WhenT Identity
type WhenId = PrimaryKey WhenT Identity
deriving instance Show When
instance Beamable WhenT
instance Beamable (PrimaryKey WhenT)
deriving instance Show (PrimaryKey WhenT Identity)

instance Table WhenT where
  data PrimaryKey WhenT f = WhenId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = WhenId . when_id

data LabelEventT f = LabelEvent
  { label_event_id       :: C f PrimaryKeyType
  , label_event_label_id :: PrimaryKey LabelT f
  , label_event_event_id :: PrimaryKey EventT f }
  deriving Generic

type LabelEvent = LabelEventT Identity
type LabelEventId = PrimaryKey LabelEventT Identity
deriving instance Show LabelEvent
instance Beamable LabelEventT
instance Beamable (PrimaryKey LabelEventT)
deriving instance Show (PrimaryKey LabelEventT Identity)

instance Table LabelEventT where
  data PrimaryKey LabelEventT f = LabelEventId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = LabelEventId . label_event_id


data UserEventT f = UserEvent
  { user_events_id         :: C f PrimaryKeyType
  , user_events_event_id   :: PrimaryKey EventT f
  , user_events_user_id    :: PrimaryKey UserT f
  , user_events_has_signed :: C f Bool
  , user_events_owner      :: PrimaryKey UserT f
  , user_events_signedHash :: C f (Maybe ByteString)
  }
  deriving Generic

type UserEvent = UserEventT Identity
type UserEventId = PrimaryKey UserEventT Identity
deriving instance Show UserEvent
instance Beamable UserEventT
instance Beamable (PrimaryKey UserEventT)
deriving instance Show (PrimaryKey UserEventT Identity)

instance Table UserEventT where
  data PrimaryKey UserEventT f = UserEventId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = UserEventId . user_events_id

{-
    hashTable   =  "CREATE TABLE IF NOT EXISTS Hashes (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID INTEGER NOT NULL, hash BLOB NOT NULL, isSigned INTEGER DEFAULT 0, signedByUserID INTEGER, keyID INTEGER DEFAULT -1,timestamp INTEGER NOT NULL);"

-}
data HashesT f = Hashes
  { hashes_id                :: C f PrimaryKeyType
  , hashes_event_id          :: PrimaryKey EventT f
  , hashes_hash              :: C f ByteString
  , hashes_is_signed         :: C f Bool
  , hashes_signed_by_user_id :: PrimaryKey UserT f
  , hashes_key_id            :: PrimaryKey KeyT f
  }
  deriving Generic
type Hashes = HashesT Identity
type HashesId = PrimaryKey HashesT Identity

-- deriving instance Show Hashes

instance Beamable HashesT
instance Beamable (PrimaryKey HashesT)
deriving instance Show (PrimaryKey HashesT Identity)

instance Table HashesT where
  data PrimaryKey HashesT f = HashesId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = HashesId . hashes_id

{- blockChainTable = "CREATE TABLE IF NOT EXISTS BlockchainTable (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID INTEGER NOT NULL, hash BLOB NOT NULL, blockChain address text NOT NULL, blockChainID INTEGER NOT NULL);"

-}
data BlockChainT f = BlockChain
  { blockchain_id         :: C f PrimaryKeyType
  , blockchain_event_id   :: PrimaryKey EventT f
  , blockchain_hash       :: C f ByteString
  , blockchain_address    :: C f Text
  , blockchain_foreign_id :: C f Integer
  } deriving Generic

type BlockChain = BlockChainT Identity
type BlockChainId = PrimaryKey BlockChainT Identity
deriving instance Show BlockChain
instance Beamable BlockChainT
instance Beamable (PrimaryKey BlockChainT)
deriving instance Show (PrimaryKey BlockChainT Identity)

instance Table BlockChainT where
  data PrimaryKey BlockChainT f = BlockChainId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = BlockChainId . blockchain_id


data SupplyChainDb f = SupplyChainDb
  { _users            :: f (TableEntity UserT)
  , _keys             :: f (TableEntity KeyT)
  , _businesses       :: f (TableEntity BusinessT)
  , _contacts         :: f (TableEntity ContactT)
  , _labels           :: f (TableEntity LabelT)
  , _what_labels      :: f (TableEntity WhatLabelT)
  , _items            :: f (TableEntity ItemT)
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
  , _hashes           :: f (TableEntity HashesT)
  , _blockchain       :: f (TableEntity BlockChainT)
  }
  deriving Generic
instance Database be SupplyChainDb

-- | Everything that comes after ``withDbModification`` is primarily
-- foreign keys that have been forced to retain their own names
supplyChainDb :: DatabaseSettings Postgres SupplyChainDb
supplyChainDb = defaultDbSettings
  `withDbModification`
  dbModification
    {
      _users =
        modifyTable (const "users") $
        tableModification
        {
          user_biz_id = BizId (fieldNamed "user_biz_id")
        }
    , _keys =
        modifyTable (const "keys") $
        tableModification
        {
          key_user_id = UserId (fieldNamed "key_user_id")
        }
    -- , _businesses =
    --     modifyTable (const "businesses") $
    --     tableModification {
    --       someField = Id (fieldNamed "short_name")
    --     }
    , _contacts =
        modifyTable (const "contacts") $
        tableModification {
         contact_user1_id = UserId (fieldNamed "contact_user1_id")
        , contact_user2_id = UserId (fieldNamed "contact_user2_id")
        }
    , _labels =
        modifyTable (const "labels") $
        tableModification {
          label_what_id = WhatId (fieldNamed "label_what_id")
        }
    , _what_labels =
        modifyTable (const "what_labels") $
        tableModification {
          what_label_what_id = WhatId (fieldNamed "what_label_what_id")
        , what_label_label_id = LabelId (fieldNamed "what_label_label_id")
        }
    , _items =
        modifyTable (const "items") $
        tableModification {
          item_label_id = LabelId (fieldNamed "item_label_id")
        }
    , _transformations =
        modifyTable (const "transformations") $
        tableModification {
          transformation_biz_id = BizId (fieldNamed "transformation_biz_id")
        }
    , _locations =
        modifyTable (const "locations") $
        tableModification {
          location_biz_id = BizId (fieldNamed "location_biz_id")
        }
    , _events =
        modifyTable (const "events") $
        tableModification {
          event_created_by = UserId (fieldNamed "event_created_by")
        }
    , _whats =
        modifyTable (const "whats") $
        tableModification {
          parent = LabelId (fieldNamed "parent")
        , what_biz_transaction_id = BizTransactionId (fieldNamed "what_biz_transaction_id")
        , what_transformation_id = TransformationId (fieldNamed "what_transformation_id")
        , what_event_id = EventId (fieldNamed "what_event_id")
        }
    , _biz_transactions =
        modifyTable (const "biz_transactions") $
        tableModification {
          biz_transaction_event_id = EventId (fieldNamed "biz_transaction_event_id")
        }
    , _whys =
        modifyTable (const "whys") $
        tableModification {
          why_event_id = EventId (fieldNamed "why_event_id")
        }
    , _wheres =
        modifyTable (const "wheres") $
        tableModification {
          where_event_id = EventId (fieldNamed "where_event_id")
        }
    , _whens =
        modifyTable (const "whens") $
        tableModification {
          when_event_id = EventId (fieldNamed "when_event_id")
        }
    , _label_events =
        modifyTable (const "label_events") $
        tableModification {
          label_event_label_id = LabelId (fieldNamed "label_event_label_id")
        , label_event_event_id = EventId (fieldNamed "label_event_event_id")
        }
    , _user_events =
        modifyTable (const "user_event") $
        tableModification {
          user_events_event_id = EventId (fieldNamed "user_events_event_id")
        , user_events_user_id = UserId (fieldNamed "user_events_user_id")
        , user_events_owner = UserId (fieldNamed "user_events_added_by")
        }
    , _hashes =
        modifyTable (const "hashes") $
        tableModification {
          hashes_event_id = EventId (fieldNamed "hashes_event_id")
        , hashes_signed_by_user_id = UserId (fieldNamed "hashes_signed_by_user_id")
        , hashes_key_id = KeyId (fieldNamed "hashes_key_id")
        }
    , _blockchain =
        modifyTable (const "blockchain") $
        tableModification {
          blockchain_event_id = EventId (fieldNamed "blockchain_event_id")
        }
    }
