{-# LANGUAGE OverloadedStrings #-}
module Migrate where

import StorageBeam -- the schemas
import Database.Beam
import Database.Beam.Postgres (Connection, Postgres, PgCommandSyntax)
import Database.Beam.Migrate.Types ( Migration
                                   , executeMigration 
                                   , CheckedDatabaseSettings
                                   , MigrationSteps
                                   , uncheckDatabase
                                   , evaluateDatabase
                                   , migrationStep )
import Database.Beam.Backend (runNoReturn)

import Data.Text (Text)
import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.SQL.Tables
import Database.Beam.Migrate.SQL.Types




migrateV1 :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres )