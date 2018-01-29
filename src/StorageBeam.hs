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
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module StorageBeam where

{-
1. convert all table definitions to under_score case
2. make the schema definitions consistent with the table definitions
3. for each primaryKey = .+Id \. .* (this is a regex), make the relevant change (eg. _itemId becomes item_id)
4. For each foreign key, make changes that appear similar to the changes made to UserT table
5. Do it for each of the tables
VSCode shortcut for multi-line cursors: Ctrl+Shift+Up/Down
-}

import           Control.Lens
import           Database.Beam as B
import           Database.Beam.Postgres
-- import           Database.PostgreSQL.Simple
-- import           Database.Beam.Backend
-- import           Database.Beam.Backend.SQL.BeamExtensions
-- import           Database.PostgreSQL.Simple.FromField
-- import           Database.Beam.Backend.SQL
-- import qualified Database.PostgreSQL.Simple.Time as PgT
import           Data.Text (Text)
-- import           Data.Int
import           Data.Time
import           Data.ByteString (ByteString)
-- import qualified Data.GS1.Event as Ev
import qualified Data.GS1.EPC as E
-- import qualified Data.GS1.DWhat as DWhat
import           Data.UUID (UUID)
import           Database.Beam.Postgres.Migrate
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.SQL.Types
import           Database.Beam.Migrate.Types
import           Data.Swagger ()
import           Servant ()

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


maxLen :: Word
maxLen = 120

-- length of the timezone offset
maxTzLen :: Word
maxTzLen = 10

pkSerialType :: DataType PgDataTypeSyntax UUID
pkSerialType = uuid

migrationStorage :: Migration PgCommandSyntax (CheckedDatabaseSettings Postgres SupplyChainDb)
migrationStorage =
  SupplyChainDb
    <$> createTable "users"
    (
      User
          (field "user_id" pkSerialType)
          (BizId (field "user_biz_id" text))
          (field "first_name" (varchar (Just maxLen)) notNull)
          (field "last_name" (varchar (Just maxLen)) notNull)
          (field "phone_number" (varchar (Just maxLen)) notNull)
          (field "password_hash" (varchar (Just maxLen)) notNull)
          (field "email_address" (varchar (Just maxLen)) notNull) -- uniqueColumn
    )
    <*> createTable "keys"
    (
      Key
          (field "key_id" pkSerialType)
          (UserId (field "key_user_id" pkSerialType))
          (field "rsa_n" text)
          (field "rsa_e" text)
          (field "creation_time" timestamptz)
          (field "revocation_time" timestamptz)
    )
    <*> createTable "businesses"
    (
      Business
          (field "biz_gs1_company_prefix" text)
          (field "biz_name" (varchar (Just maxLen)) notNull)
          (field "biz_function" (varchar (Just maxLen)) notNull)
          (field "biz_site_name" (varchar (Just maxLen)) notNull)
          (field "biz_address" (varchar (Just maxLen)) notNull)
          (field "biz_lat" double)
          (field "biz_long" double)
    )
    <*> createTable "contacts"
    (
      Contact
          (field "contact_id" pkSerialType)
          (UserId (field "contact_user1_id" pkSerialType))
          (UserId (field "contact_user2_id" pkSerialType))
    )
    <*> createTable "labels"
    (
      Label
          (field "label_id" pkSerialType)
          (field "label_type" (varchar (Just maxLen)) notNull)
          (WhatId (field "label_what_id" pkSerialType))
          (field "label_gs1_company_prefix" (varchar (Just maxLen)) notNull)
          (field "item_reference" (varchar (Just maxLen)) notNull)
          (field "serial_number" (varchar (Just maxLen)) notNull)
          (field "state" (varchar (Just maxLen)) notNull)
          (field "lot" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "items"
    (
      Item
          (field "item_id" pkSerialType)
          (LabelId (field "item_label_id" pkSerialType))
          (field "item_description" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "transformations"
    (
      Transformation
          (field "transformation_id" pkSerialType)
          (field "transformation_description" (varchar (Just maxLen)) notNull)
          (BizId (field "transformation_biz_id" text))
    )
    <*> createTable "locations"
    (
      Location
          (field "location_id" pkSerialType)
          (BizId (field "location_biz_id" text))
          (field "location_lat" double)
          (field "location_long" double)
    )
    <*> createTable "events"
    (
      Event
          (field "event_id" pkSerialType)
          (field "foreign_event_id" (varchar (Just maxLen)) notNull)
          (BizId (field "event_label_id" text))
          (UserId (field "event_created_by" pkSerialType))
          (field "json_event" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "whats"
    (
      What
          (field "what_id" pkSerialType)
          (field "what_type" text)
          (field "action" text)
          (LabelId (field "parent" pkSerialType))
          -- (field "input" bigserial)
          -- (field "output" bigserial)
          (BizTransactionId (field "what_biz_transaction_id" pkSerialType))
          (TransformationId (field "what_transformation_id" pkSerialType))
          (EventId (field "what_event_id" pkSerialType))
    )
    <*> createTable "bizTransactions"
    (
      BizTransaction
          (field "biz_transaction_id" pkSerialType)
          (field "biz_transaction_type_id" (varchar (Just maxLen)))
          (field "biz_transaction_id_urn" (varchar (Just maxLen)))
          (EventId (field "biz_transaction_event_id" pkSerialType))
    )
    <*> createTable "whys"
    (
      Why
          (field "why_id" pkSerialType)
          (field "biz_step" text)
          (field "disposition" text)
          (EventId (field "why_event_id" pkSerialType))
    )
    <*> createTable "wheres"
    (
      Where
          (field "where_id" pkSerialType)
          (LocationId (field "read_point" pkSerialType))
          (LocationId (field "biz_location" pkSerialType))
          (field "src_type" text)
          (field "dest_type" text)
          (EventId (field "where_event_id" pkSerialType))
    )
    <*> createTable "whens"
    (
      When
          (field "when_id" pkSerialType notNull)
          (field "event_time" timestamptz notNull)
          (field "record_time" (maybeType timestamptz))
          (field "time_zone" (varchar (Just maxTzLen)) notNull)
          (EventId (field "when_event_id" pkSerialType))
    )
    <*> createTable "labelEvents"
    (
      LabelEvent
          (field "label_event_id" pkSerialType)
          (LabelId (field "label_event_label_id" pkSerialType))
          (EventId (field "label_event_event_id" pkSerialType))
    )

    -- note that all ADDITIONAL TABLES have all fields as NOT NULL
    -- representing bytestring?
    <*> createTable "userEvents"
    (
      UserEvents
          (field "user_events_id" pkSerialType notNull)
          (EventId (field "user_events_event_id" pkSerialType notNull))
          (UserId (field "user_events_user_id" pkSerialType notNull))
          (field "user_events_has_signed" boolean notNull)
          (UserId (field "user_events_added_by" pkSerialType notNull))
          (field "user_events_signedHash" bytea notNull)
    )
    <*> createTable "hashes"
    (
      Hashes
          (field "hashes_id" pkSerialType notNull)
          (EventId (field "hashes_event_id" pkSerialType notNull))
          (field "hashes_hash" bytea notNull)
          (field "hashes_is_signed" boolean notNull)
          (UserId (field "hashes_signed_by_user_id" pkSerialType notNull))
          (KeyId (field "hashes_key_id" pkSerialType notNull))
    )
    <*> createTable "blockchain"
    (
      BlockChain
          (field "blockchain_id" pkSerialType notNull)
          (EventId (field "blockchain_event_id" pkSerialType notNull))
          (field "blockchain_hash" bytea notNull)
          (field "blockchain_address" text notNull)
          -- can use int or smallInt here because Integer is instance of Integral
          -- note: Database.Beam.Migrate.SQL.Types
          (field "blockchain_foreign_id" int notNull)
    )


data UserT f = User
  { user_id              :: C f PrimaryKeyType
  , user_biz_id          :: PrimaryKey BusinessT f
  , first_name           :: C f Text
  , last_name            :: C f Text
  , phone_number         :: C f Text
  , password_hash        :: C f Text --XXX - should this be blob?
  , email_address        :: C f Text }
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

data KeyT f = Key
  { key_id             :: C f PrimaryKeyType
  , key_user_id        :: PrimaryKey UserT f
  , rsa_n              :: C f Text --XXX should this be Int64?
  , rsa_e              :: C f Text -- as above
  , creationTime       :: C f LocalTime -- UTCTime
  , revocationTime     :: C f LocalTime -- UTCTime
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

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 11

data BusinessT f = Business
  { biz_gs1CompanyPrefix  :: C f E.GS1CompanyPrefix -- PrimaryKey
  , biz_name              :: C f Text
  , biz_function          :: C f Text
  , biz_siteName          :: C f Text
  , biz_address           :: C f Text
  , biz_lat               :: C f Double
  , biz_long              :: C f Double }
  deriving Generic
type Business = BusinessT Identity
type BizId = PrimaryKey BusinessT Identity

deriving instance Show Business

instance Beamable BusinessT
instance Beamable (PrimaryKey BusinessT)
deriving instance Show (PrimaryKey BusinessT Identity)

instance Table BusinessT where
  data PrimaryKey BusinessT f = BizId (C f E.GS1CompanyPrefix)
    deriving Generic
  primaryKey = BizId . biz_gs1CompanyPrefix

data ContactT f = Contact
  { contact_id                :: C f PrimaryKeyType
  , contact_user1_id           :: PrimaryKey UserT f
  , contact_user2_id           :: PrimaryKey UserT f }
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
  , label_type               :: C f Text -- input/output/parent
  , label_what_id            :: PrimaryKey WhatT f
  , label_gs1_company_prefix :: C f Text --should this be bizId instead?
  , item_reference           :: C f Text
  , serial_number            :: C f Text
  , state                    :: C f Text
  -- , label_type               :: C f Text
  , lot                      :: C f Text }
  deriving Generic
type Label = LabelT Identity
type LabelId = PrimaryKey LabelT Identity

deriving instance Show Label

instance Beamable LabelT
instance Beamable (PrimaryKey LabelT)
deriving instance Show (PrimaryKey LabelT Identity)

instance Table LabelT where
  data PrimaryKey LabelT f = LabelId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = LabelId . label_id

data ItemT f = Item
  { item_id            :: C f PrimaryKeyType
  , item_label_id      :: PrimaryKey LabelT f
  , item_description   :: C f Text }
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
  { transformation_id           :: C f PrimaryKeyType
  , transformation_description  :: C f Text
  , transformation_biz_id       :: PrimaryKey BusinessT f }
  deriving Generic
type Transformation = TransformationT Identity
type TransformationId = PrimaryKey TransformationT Identity

deriving instance Show Transformation

instance Beamable TransformationT
instance Beamable (PrimaryKey TransformationT)
deriving instance Show (PrimaryKey TransformationT Identity)

instance Table TransformationT where
  data PrimaryKey TransformationT f = TransformationId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = TransformationId . transformation_id

data LocationT f = Location
  { location_id                 :: C f PrimaryKeyType
  , location_biz_id             :: PrimaryKey BusinessT f
  , location_lat                :: C f Double
  , location_long               :: C f Double }
  deriving Generic

type Location = LocationT Identity
type LocationId = PrimaryKey LocationT Identity

deriving instance Show Location

instance Beamable LocationT
instance Beamable (PrimaryKey LocationT)
deriving instance Show (PrimaryKey LocationT Identity)

instance Table LocationT where
  data PrimaryKey LocationT f = LocationId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = LocationId . location_id

data EventT f = Event
  { event_id                    :: C f PrimaryKeyType
  , foreign_event_id             :: C f Text -- Event ID from XML from foreign systems.
  , event_label_id               :: PrimaryKey BusinessT f --the label scanned to generate this event.
  , event_created_by             :: PrimaryKey UserT f
  , json_event                  :: C f Text }
  deriving Generic
type Event = EventT Identity
type EventId = PrimaryKey EventT Identity

deriving instance Show Event

instance Beamable EventT
instance Beamable (PrimaryKey EventT)
deriving instance Show (PrimaryKey EventT Identity)

instance Table EventT where
  data PrimaryKey EventT f = EventId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = EventId . event_id

data WhatT f = What
  { what_id                    :: C f PrimaryKeyType
  , what_type                  :: C f Text -- Ev.EventType
  , action                     :: C f Text -- E.Action
  , parent                     :: PrimaryKey LabelT f
  -- , input                      :: C f [LabelEPC]
  -- , output                     :: C f [LabelEPC]
  , what_biz_transaction_id    :: PrimaryKey BizTransactionT f
  , what_transformation_id     :: PrimaryKey TransformationT f
  , what_event_id              :: PrimaryKey EventT f }
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


data BizTransactionT f = BizTransaction
  { biz_transaction_id          :: C f PrimaryKeyType
  , biz_transaction_type_id     :: C f Text
  , biz_transaction_id_urn      :: C f Text
  , biz_transaction_event_id    :: PrimaryKey EventT f }

  deriving Generic

type BizTransaction = BizTransactionT Identity
type BizTransactionId = PrimaryKey BizTransactionT Identity

deriving instance Show BizTransaction
instance Beamable BizTransactionT

instance Beamable (PrimaryKey BizTransactionT)
deriving instance Show (PrimaryKey BizTransactionT Identity)

instance Table BizTransactionT where
  data PrimaryKey BizTransactionT f = BizTransactionId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = BizTransactionId . biz_transaction_id


data WhyT f = Why
  { why_id                      :: C f PrimaryKeyType
  , biz_step                    :: C f Text -- E.BizStep
  , disposition                 :: C f Text -- E.Disposition
  , why_event_id                :: PrimaryKey EventT f }

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
  { where_id                    :: C f PrimaryKeyType
  , read_point                  :: PrimaryKey LocationT f
  , biz_location                :: PrimaryKey LocationT f
  , src_type                    :: C f Text -- E.SourceDestType
  , dest_type                   :: C f Text -- E.SourceDestType
  , where_event_id              :: PrimaryKey EventT f }

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

type OffsetString = Text

data WhenT f = When
  { when_id                      :: C f PrimaryKeyType
  , event_time                   :: C f LocalTime
  , record_time                  :: C f (Maybe LocalTime)
  , time_zone                    :: C f OffsetString -- TimeZone
  -- call unpack . timeZoneOffsetString on the TimeZone object
  -- to put it in the db
  , when_event_id                :: PrimaryKey EventT f }
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
  { label_event_id               :: C f PrimaryKeyType
  , label_event_label_id         :: PrimaryKey LabelT f
  , label_event_event_id         :: PrimaryKey EventT f }
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


-- ADDITIONAL TABLES
data UserEventsT f = UserEvents
  { user_events_id         :: C f PrimaryKeyType
  , user_events_event_id   :: PrimaryKey EventT f
  , user_events_user_id    :: PrimaryKey UserT f
  , user_events_has_signed :: C f Bool
  , user_events_added_by   :: PrimaryKey UserT f
  , user_events_signedHash :: C f ByteString
  }
  deriving Generic

type UserEvents = UserEventsT Identity
type UserEventsId = PrimaryKey UserEventsT Identity
deriving instance Show UserEvents
instance Beamable UserEventsT
instance Beamable (PrimaryKey UserEventsT)
deriving instance Show (PrimaryKey UserEventsT Identity)

instance Table UserEventsT where
  data PrimaryKey UserEventsT f = UserEventsId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = UserEventsId . user_events_id

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
-- END OF ADDITIONAL TABLES

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
  , _bizTransactions :: f (TableEntity BizTransactionT)
  , _whys            :: f (TableEntity WhyT)
  , _wheres          :: f (TableEntity WhereT)
  , _whens           :: f (TableEntity WhenT)
  , _labelEvents     :: f (TableEntity LabelEventT)
  , _userEvents      :: f (TableEntity UserEventsT)
  , _hashes          :: f (TableEntity HashesT)
  , _blockchain      :: f (TableEntity BlockChainT)
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
    -- , _labels =
    --     modifyTable (const "labels") $
    --     tableModification {
    --       someField = Id (fieldNamed "short_name")
    --     }
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
          event_label_id = BizId (fieldNamed "event_label_id")
        , event_created_by = UserId (fieldNamed "event_created_by")
        }
    , _whats =
        modifyTable (const "whats") $
        tableModification {
          parent = LabelId (fieldNamed "parent")
        , what_biz_transaction_id = BizTransactionId (fieldNamed "what_biz_transaction_id")
        , what_transformation_id = TransformationId (fieldNamed "what_transformation_id")
        , what_event_id = EventId (fieldNamed "what_event_id")
        }
    , _bizTransactions =
        modifyTable (const "bizTransactions") $
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
          read_point = LocationId (fieldNamed "read_point")
        , biz_location = LocationId (fieldNamed "biz_location")
        , where_event_id = EventId (fieldNamed "where_event_id")
        }
    , _whens =
        modifyTable (const "whens") $
        tableModification {
          when_event_id = EventId (fieldNamed "when_event_id")
        }
    , _labelEvents =
        modifyTable (const "labelEvents") $
        tableModification {
          label_event_label_id = LabelId (fieldNamed "label_event_label_id")
        , label_event_event_id = EventId (fieldNamed "label_event_event_id")
        }
    -- all the foreign keys are relevant here
    , _userEvents =
        modifyTable (const "userEvents") $
        tableModification {
          user_events_event_id = EventId (fieldNamed "user_events_event_id")
        , user_events_user_id = UserId (fieldNamed "user_events_user_id")
        , user_events_added_by = UserId (fieldNamed "user_events_added_by")
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
