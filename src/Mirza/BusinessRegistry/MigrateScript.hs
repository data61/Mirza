{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- TODO: Remove the following and fix the orphand instances properly.
--       (This is just to work around that we don't want to spend the time
--       fixing these currently and don't want these warnings to show up until
--       we are ready to deal with this properly.)
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Contains the migration function of ``businessRegistryDb``
module Mirza.BusinessRegistry.MigrateScript (migrationStorage) where

import           Mirza.BusinessRegistry.StorageBeam
import           Mirza.Common.Beam
import           Mirza.Common.GS1BeamOrphans

import qualified Data.GS1.EPC                         as EPC

import qualified Database.Beam                        as B
import qualified Database.Beam.Backend.SQL            as BSQL
import qualified Database.Beam.Migrate                as BMigrate
import           Database.Beam.Migrate.SQL            (DataType)
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres               as BPostgres
import           Database.Beam.Postgres.Syntax        (PgDataTypeSyntax)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField   (ToField (..))

import           Data.UUID                            (UUID)




maxLen :: Word
maxLen = 120

pkSerialType :: DataType PgDataTypeSyntax UUID
pkSerialType = uuid

migrationStorage :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres BusinessRegistryDB)
migrationStorage () =
  BusinessRegistryDB
    <$> createTable "users"
    (
      UserT
          (field "user_id" pkSerialType)
          (BizId (field "user_biz_id" gs1CompanyPrefixType))
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
            (field "biz_gs1_company_prefix" gs1CompanyPrefixType) -- note is primary key
            (field "biz_name" (varchar (Just maxLen)) notNull)
            (field "biz_function" (varchar (Just maxLen)) notNull)
            (field "biz_site_name" (varchar (Just maxLen)) notNull)
            (field "biz_address" (varchar (Just maxLen)) notNull)
            (field "biz_lat" double)
            (field "biz_long" double)
      )
