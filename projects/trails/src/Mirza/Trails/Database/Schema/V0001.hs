{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module contains all the table definitions
-- Convention: Table types and constructors are suffixed with T (for Table).
module Mirza.Trails.Database.Schema.V0001 where


import           Mirza.Trails.Types

import           Mirza.Common.Beam              (defaultFkConstraint,
                                                 lastUpdateField)
import           Mirza.Common.GS1BeamOrphans

import           Data.GS1.EPC                   (GS1CompanyPrefix)

import           Database.Beam                  as B
import           Database.Beam.Migrate.SQL      as BSQL
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres         (Postgres)
import           Database.Beam.Postgres.Migrate (uuid)

import           Control.Lens

import           Data.Text
import           Data.Time                      (LocalTime)
import           Data.UUID                      (UUID)

import           GHC.Generics                   (Generic)


-- Convention: Table types and constructors are suffixed with T (for Table).

-- Database
data TrailsDB f = TrailsDB
  { _entries :: f (TableEntity EntriesT)
  , _parents :: f (TableEntity ParentsT)
  }
  deriving Generic
instance Database anybackend TrailsDB


-- Migration: Intialisation -> V1.
migration :: () -> Migration Postgres (CheckedDatabaseSettings Postgres TrailsDB)
migration () = do
  entriesT <- createTable entriesTName $
    EntriesT (field entriesTFieldSignature signatureType notNull)
             (field entriesTFieldTimestamp timestamp notNull)
             (field entriesTFieldGS1CompanyPrefix gs1CompanyPrefixFieldType notNull)
             (field entriesTFieldEventId uuid notNull)
             lastUpdateField

  parentsT <- createTable parentsTName $
    ParentsT (EntriesPrimaryKey $ field parentsTFieldSignature signatureType notNull (defaultFkConstraint entriesTName [entriesTFieldSignature]))
             (field parentsTFieldParentSignature signatureType notNull)

  pure $ TrailsDB entriesT parentsT

-- Table names
entriesTName :: Text
entriesTName = "entries"
entriesTFieldSignature :: Text
entriesTFieldSignature = "entries_signature"
entriesTFieldTimestamp :: Text
entriesTFieldTimestamp = "entries_timestanp"
entriesTFieldGS1CompanyPrefix :: Text
entriesTFieldGS1CompanyPrefix = "entries_gs1_company_prefix"
entriesTFieldEventId :: Text
entriesTFieldEventId = "entries_event_id"

parentsTName :: Text
parentsTName = "parents"
parentsTFieldSignature :: Text
parentsTFieldSignature = "parents_" <> entriesTFieldSignature
parentsTFieldParentSignature :: Text
parentsTFieldParentSignature = "parents_parent_signature"


--------------------------------------------------------------------------------
-- Entries table
--------------------------------------------------------------------------------

type Entries = EntriesT Identity
deriving instance Show Entries

data EntriesT f = EntriesT
  { entries_signature         :: C f SignaturePlaceholder
  , entries_timestamp         :: C f LocalTime
  , entries_gs1company_prefix :: C f GS1CompanyPrefix
  , entries_event_id          :: C f UUID
  , entries_last_update       :: C f (Maybe LocalTime)
  } deriving Generic

type EntriesPrimaryKey = PrimaryKey EntriesT Identity
deriving instance Show (PrimaryKey EntriesT Identity)

instance Beamable EntriesT
instance Beamable (PrimaryKey EntriesT)

instance Table EntriesT where
  data PrimaryKey EntriesT f = EntriesPrimaryKey (C f SignaturePlaceholder)
    deriving Generic
  primaryKey = EntriesPrimaryKey . entries_signature
deriving instance Eq (PrimaryKey EntriesT Identity)


--------------------------------------------------------------------------------
-- Parent table
--------------------------------------------------------------------------------

type Parents = ParentsT Identity
deriving instance Show Parents

data ParentsT f = ParentsT
  { parents_entry_signature  :: PrimaryKey EntriesT f
  , parents_parent_signature :: C f SignaturePlaceholder
  } deriving Generic

type ParentsPrimaryKey = PrimaryKey ParentsT Identity
deriving instance Show (PrimaryKey ParentsT Identity)

instance Beamable ParentsT
instance Beamable (PrimaryKey ParentsT)

instance Table ParentsT where
  data PrimaryKey ParentsT f = ParentMapping (PrimaryKey EntriesT f) (C f SignaturePlaceholder)
    deriving Generic
  primaryKey = ParentMapping <$> parents_entry_signature <*> parents_parent_signature
deriving instance Eq (PrimaryKey ParentsT Identity)
