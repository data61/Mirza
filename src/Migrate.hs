{-# LANGUAGE OverloadedStrings #-}
module Migrate where

import StorageBeam -- the schemas
import Database.Beam (withDatabaseDebug)
import Database.Beam.Postgres (Connection)
import Database.Beam.Migrate.Types (Migration, executeMigration)
import Database.Beam.Backend (runNoReturn)




