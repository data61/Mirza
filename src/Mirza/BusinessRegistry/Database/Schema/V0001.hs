{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

-- | This module contains all the table definitions
-- The migration script has been moved to the module MigrateScript
-- If some definition is changed here, please make the equivalent change
-- in MigrateScript
module Mirza.BusinessRegistry.Database.Schema.V0001 where

import qualified Data.GS1.EPC                         as EPC

import           Mirza.Common.GS1BeamOrphans

import           Control.Lens

import           Data.ByteString                      (ByteString)
import           Data.Text                            (Text)
import           Data.Time                            (LocalTime)
import           Data.UUID                            (UUID)

import           Database.Beam                        as B
import qualified Database.Beam.Backend.SQL            as BSQL
import qualified Database.Beam.Migrate                as BMigrate
import           Database.Beam.Migrate.SQL            (DataType)
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax        (PgDataTypeSyntax)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField   (ToField (..))

import           Data.Swagger


type PrimaryKeyType = UUID

type UserID = PrimaryKey UserT Identity
instance ToSchema UserID
instance ToParamSchema UserID


-- Table types and constructors are suffixed with T (for Table).

data BusinessRegistryDB f = BusinessRegistryDB
  { _users      :: f (TableEntity UserT)
  , _keys       :: f (TableEntity KeyT)
  , _businesses :: f (TableEntity BusinessT)
  }
  deriving Generic
instance Database anybackend BusinessRegistryDB


businessRegistryDB :: DatabaseSettings Postgres BusinessRegistryDB
businessRegistryDB = defaultDbSettings
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
    , _businesses =
        modifyTable (const "businesses") $
        tableModification
        -- {
        --   biz_gs1_company_prefix = BizId (fieldNamed "biz_company_prefix")
        -- }
    }


maxLen :: Word
maxLen = 120

pkSerialType :: DataType PgDataTypeSyntax UUID
pkSerialType = uuid

migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres BusinessRegistryDB)
migration () =
  BusinessRegistryDB
    <$> createTable "users"
    (
      UserT
          (field "user_id" pkSerialType)
          (BizId (field "user_biz_id" pkSerialType))
          (field "first_name" (varchar (Just maxLen)) notNull)
          (field "last_name" (varchar (Just maxLen)) notNull)
          (field "phone_number" (varchar (Just maxLen)) notNull)
          (field "password_hash" binaryLargeObject notNull)
          (field "email_address" (varchar (Just maxLen)) unique)
    )
    <*> createTable "keys"
    (
      KeyT
          (field "key_id" pkSerialType)
          (UserId (field "key_user_id" pkSerialType))
          (field "pem_str" text)
          (field "creation_time" timestamptz)
          (field "revocation_time" (maybeType timestamptz))
          (field "expiration_time" (maybeType timestamptz))
    )
    <*> createTable "businesses"
      (
        BusinessT
            (field "business_id" pkSerialType)
            (field "biz_gs1_company_prefix" gs1CompanyPrefixType)
            (field "biz_name" (varchar (Just maxLen)) notNull)
            (field "biz_function" (varchar (Just maxLen)) notNull)
            (field "biz_site_name" (varchar (Just maxLen)) notNull)
            (field "biz_address" (varchar (Just maxLen)) notNull)
            (field "biz_lat" double)
            (field "biz_long" double)
      )


data UserT f = UserT
  { user_id       :: C f PrimaryKeyType
  , user_biz_id   :: PrimaryKey BusinessT f
  , first_name    :: C f Text
  , last_name     :: C f Text
  , phone_number  :: C f Text
  , password_hash :: C f ByteString
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

data KeyT f = KeyT
  { key_id          :: C f PrimaryKeyType
  , key_user_id     :: PrimaryKey UserT f
  , pem_str         :: C f Text
  , creation_time   :: C f LocalTime -- UTCTime
  , revocation_time :: C f (Maybe LocalTime) -- UTCTime
  , expiration_time :: C f (Maybe LocalTime) -- UTCTime
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

data BusinessT f = BusinessT
  { business_id            :: C f PrimaryKeyType
  , biz_gs1_company_prefix :: C f EPC.GS1CompanyPrefix
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
  data PrimaryKey BusinessT f = BizId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = BizId . business_id
deriving instance Eq (PrimaryKey BusinessT Identity)
