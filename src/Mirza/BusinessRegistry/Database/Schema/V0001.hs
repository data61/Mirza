{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

-- | This module contains all the table definitions
-- The migration script has been moved to the module MigrateScript
-- If some definition is changed here, please make the equivalent change
-- in MigrateScript
module Mirza.BusinessRegistry.Database.Schema.V0001 where

import qualified Data.GS1.EPC                     as EPC

import           Mirza.Common.GS1BeamOrphans

import           Control.Lens

import           Data.ByteString                  (ByteString)
import           Data.Text                        (Text)
import           Data.Time                        (LocalTime)
import           Data.UUID                        (UUID)

import           Database.Beam                    as B
import           Database.Beam.Migrate.SQL        (DataType)
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax    (PgDataTypeSyntax)

import           Data.Aeson
import           Data.Swagger



-- Convention: Table types and constructors are suffixed with T (for Table).


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

defaultFieldMaxLength :: Word
defaultFieldMaxLength = 120


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

type PrimaryKeyType = UUID


--------------------------------------------------------------------------------
-- Database
--------------------------------------------------------------------------------

pkSerialType :: DataType PgDataTypeSyntax UUID
pkSerialType = uuid


-- Database
data BusinessRegistryDB f = BusinessRegistryDB
  { _users      :: f (TableEntity UserT)
  , _businesses :: f (TableEntity BusinessT)
  , _keys       :: f (TableEntity KeyT)
  }
  deriving Generic
instance Database anybackend BusinessRegistryDB


-- Migration: Intialisation -> V1.
migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres BusinessRegistryDB)
migration () =
  BusinessRegistryDB
    <$> createTable "users"
    (
      UserT
          (field "user_id" pkSerialType)
          (BizId (field "user_biz_id" pkSerialType))
          (field "first_name" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "last_name" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "phone_number" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "password_hash" binaryLargeObject notNull)
          (field "email_address" (varchar (Just defaultFieldMaxLength)) unique notNull)
    )
    <*> createTable "businesses"
      (
        BusinessT
            (field "business_id" pkSerialType)
            (field "biz_gs1_company_prefix" gs1CompanyPrefixType)
            (field "biz_name" (varchar (Just defaultFieldMaxLength)) notNull)
            (field "biz_function" (varchar (Just defaultFieldMaxLength)) notNull)
            (field "biz_site_name" (varchar (Just defaultFieldMaxLength)) notNull)
            (field "biz_address" (varchar (Just defaultFieldMaxLength)) notNull)
            (field "biz_lat" double)
            (field "biz_long" double)
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


--------------------------------------------------------------------------------
-- User table.
--------------------------------------------------------------------------------

type User = UserT Identity
deriving instance Show User

data UserT f = UserT
  { user_id       :: C f PrimaryKeyType
  , user_biz_id   :: PrimaryKey BusinessT f
  , first_name    :: C f Text
  , last_name     :: C f Text
  , phone_number  :: C f Text
  , password_hash :: C f ByteString
  , email_address :: C f Text }
  deriving Generic

type UserID = PrimaryKey UserT Identity
deriving instance Show (PrimaryKey UserT Identity)
instance ToSchema UserID
instance ToParamSchema UserID
instance ToJSON (PrimaryKey UserT Identity) where
  toJSON (UserId uid) = toJSON uid
instance FromJSON (PrimaryKey UserT Identity) where
  parseJSON = fmap UserId . parseJSON

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = UserId . user_id
deriving instance Eq (PrimaryKey UserT Identity)


--------------------------------------------------------------------------------
-- Business Table
--------------------------------------------------------------------------------

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 11

type Business = BusinessT Identity
deriving instance Show Business

data BusinessT f = BusinessT
  { business_id            :: C f PrimaryKeyType
  , biz_gs1_company_prefix :: C f EPC.GS1CompanyPrefix
  , biz_name               :: C f Text
  , biz_function           :: C f Text
  , biz_site_name          :: C f Text
  , biz_address            :: C f Text
  , biz_lat                :: C f Double
  , biz_long               :: C f Double
  }
  deriving Generic

type BizId = PrimaryKey BusinessT Identity
deriving instance Show (PrimaryKey BusinessT Identity)

instance Beamable BusinessT
instance Beamable (PrimaryKey BusinessT)

instance Table BusinessT where
  data PrimaryKey BusinessT f = BizId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = BizId . business_id
deriving instance Eq (PrimaryKey BusinessT Identity)


--------------------------------------------------------------------------------
-- Keys Table
--------------------------------------------------------------------------------

type Key = KeyT Identity
deriving instance Show Key

data KeyT f = KeyT
  { key_id          :: C f PrimaryKeyType
  , key_user_id     :: PrimaryKey UserT f
  , pem_str         :: C f Text
  , creation_time   :: C f LocalTime -- UTCTime
  , revocation_time :: C f (Maybe LocalTime) -- UTCTime
  , expiration_time :: C f (Maybe LocalTime) -- UTCTime
  }
  deriving Generic

type KeyId = PrimaryKey KeyT Identity
deriving instance Show (PrimaryKey KeyT Identity)

instance Beamable KeyT
instance Beamable (PrimaryKey KeyT)

instance Table KeyT where
  data PrimaryKey KeyT f = KeyId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = KeyId . key_id
deriving instance Eq (PrimaryKey KeyT Identity)
