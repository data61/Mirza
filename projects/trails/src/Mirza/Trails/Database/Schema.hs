{-# LANGUAGE OverloadedStrings #-}


module Mirza.Trails.Database.Schema
  ( module Current
  , migration
  , trailsDB
  , checkedTrailsDB
  , primaryKey
  ) where


import           Mirza.Trails.Database.Schema.V0001 as Current hiding
                                                                (migration)

import qualified Mirza.Trails.Database.Schema.V0001 as V0001 (migration)
-- import qualified Mirza.Trails.Database.Schema.V0002 as V0002 (migration)

import           Database.Beam                      (DatabaseSettings)
import           Database.Beam.Migrate.Types        hiding (migrateScript)
import           Database.Beam.Postgres             (Postgres)
import           Database.Beam.Schema.Tables        (primaryKey)

--import           Control.Arrow                           ( (>>>) )


migration :: MigrationSteps Postgres () (CheckedDatabaseSettings Postgres Current.TrailsDB)
migration = migrationStep "Initial commit"      V0001.migration
--        >>> migrationStep "TBA" V0002.migration

trailsDB :: DatabaseSettings Postgres Current.TrailsDB
trailsDB = unCheckDatabase checkedTrailsDB

checkedTrailsDB :: CheckedDatabaseSettings Postgres Current.TrailsDB
checkedTrailsDB = evaluateDatabase migration
