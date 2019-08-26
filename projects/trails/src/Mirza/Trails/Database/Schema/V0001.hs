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

import           Data.Text
import           Data.Time                      (LocalTime)
import           Data.UUID                      (UUID)

import           GHC.Generics                   (Generic)


-- Convention: Table types and constructors are suffixed with T (for Table).

-- Database
data TrailsDB f = TrailsDB
  { _entries  :: f (TableEntity EntriesT)
  , _previous :: f (TableEntity PreviousT)
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

  previousT <- createTable previousTName $
    PreviousT (EntriesPrimaryKey $ field previousTFieldSignature signatureType notNull (defaultFkConstraint entriesTName [entriesTFieldSignature]))
             (field previousTFieldPreviousSignature signatureType notNull)

  pure $ TrailsDB entriesT previousT

-- Table names
entriesTName :: Text
entriesTName = "entries"
entriesTFieldSignature :: Text
entriesTFieldSignature = entriesTName <> "_signature"
entriesTFieldTimestamp :: Text
entriesTFieldTimestamp = entriesTName <> "_timestanp"
entriesTFieldGS1CompanyPrefix :: Text
entriesTFieldGS1CompanyPrefix = entriesTName <> "_gs1_company_prefix"
entriesTFieldEventId :: Text
entriesTFieldEventId = entriesTName <> "_event_id"

previousTName :: Text
previousTName = "previous"
previousTFieldSignature :: Text
previousTFieldSignature = previousTName <> "_" <> entriesTFieldSignature
previousTFieldPreviousSignature :: Text
previousTFieldPreviousSignature = previousTName <> "previous_signature"


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


entriesPrimaryKeyToSignature :: EntriesPrimaryKey -> SignaturePlaceholder
entriesPrimaryKeyToSignature (EntriesPrimaryKey sig) = sig


--------------------------------------------------------------------------------
-- Previous table
--------------------------------------------------------------------------------

type Previous = PreviousT Identity
deriving instance Show Previous

data PreviousT f = PreviousT
  { previous_entry_signature    :: PrimaryKey EntriesT f
  , previous_previous_signature :: C f SignaturePlaceholder -- Note: The naming convention is table and then field name so it looks weird but its the previous table and its the previous_signature field.
  } deriving Generic

type PreviousPrimaryKey = PrimaryKey PreviousT Identity
deriving instance Show (PrimaryKey PreviousT Identity)

instance Beamable PreviousT
instance Beamable (PrimaryKey PreviousT)

instance Table PreviousT where
  data PrimaryKey PreviousT f = PreviousMapping (PrimaryKey EntriesT f) (C f SignaturePlaceholder)
    deriving Generic
  primaryKey = PreviousMapping <$> previous_entry_signature <*> previous_previous_signature
deriving instance Eq (PrimaryKey PreviousT Identity)
