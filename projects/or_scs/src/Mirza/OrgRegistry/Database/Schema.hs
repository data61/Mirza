module Mirza.OrgRegistry.Database.Schema
  ( module Current
  , migration
  , orgRegistryDB
  , checkedOrgRegistryDB
  , primaryKey
  ) where

--import           Control.Arrow                           ((>>>))

import           Database.Beam                           (DatabaseSettings)
import           Database.Beam.Migrate.Types             hiding (migrateScript)
import           Database.Beam.Postgres                  (PgCommandSyntax,
                                                          Postgres)
import           Database.Beam.Schema.Tables             (primaryKey)

import           Mirza.OrgRegistry.Database.Schema.V0001 as Current hiding
                                                                     (migration)

import qualified Mirza.OrgRegistry.Database.Schema.V0001 as V0001 (migration)
--import qualified Mirza.OrgRegistry.Database.Schema.V0002 as V0002 (migration)


migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres Current.OrgRegistryDB)
migration = migrationStep "Initial commit"      V0001.migration
--        >>> migrationStep "Add LocationT table" V0002.migration

orgRegistryDB :: DatabaseSettings Postgres Current.OrgRegistryDB
orgRegistryDB = unCheckDatabase checkedOrgRegistryDB

checkedOrgRegistryDB :: CheckedDatabaseSettings Postgres Current.OrgRegistryDB
checkedOrgRegistryDB = evaluateDatabase migration
