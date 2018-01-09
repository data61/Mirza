{-# LANGUAGE OverloadedStrings #-}
module Migrate where

import StorageBeam -- the schemas
<<<<<<< HEAD
import Database.Beam
import Database.Beam.Postgres (Connection, Postgres, PgCommandSyntax)
import Database.Beam.Migrate.Types ( Migration
                                   , executeMigration 
                                   , CheckedDatabaseSettings
                                   , MigrationSteps
                                   , uncheckDatabase
                                   , evaluateDatabase
                                   , migrationStep )
=======
import Database.Beam (withDatabaseDebug)
import Database.Beam.Postgres (Connection)
import Database.Beam.Migrate.Types
>>>>>>> 658660d01c1eb000390d6faed0697c54da4893df
import Database.Beam.Backend (runNoReturn)

import Data.Text (Text)
import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Migrate.SQL.Types




migrateV1 :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres )