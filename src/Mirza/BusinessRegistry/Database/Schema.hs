

module Mirza.BusinessRegistry.Database.Schema
  ( module Current
  , migration, businessRegistryDB ) where

import qualified Mirza.BusinessRegistry.Database.Schema.V0001 as Current

import qualified Mirza.BusinessRegistry.Database.Schema.V0001 as V0001 (businessRegistryDB,
                                                                        migration)
-- import qualified Mirza.BusinessRegistry.Database.Schema.V0002 as V0002 (businessRegistryDB, migration)

-- import           Control.Arrow ((>>>))

import           Database.Beam                                (DatabaseSettings)
import           Database.Beam.Migrate.Types                  (CheckedDatabaseSettings,
                                                               MigrationSteps,
                                                               evaluateDatabase,
                                                               migrationStep,
                                                               unCheckDatabase)
import           Database.Beam.Postgres                       (PgCommandSyntax,
                                                               Postgres)

migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres Current.BusinessRegistryDB)
migration = migrationStep "Initial commit" V0001.migration
           -- >>> migrationStep """todo comment""" V0002.migration

businessRegistryDB :: DatabaseSettings Postgres Current.BusinessRegistryDB
businessRegistryDB = unCheckDatabase checkedBusinessRegistryDB

checkedBusinessRegistryDB :: CheckedDatabaseSettings Postgres Current.BusinessRegistryDB
checkedBusinessRegistryDB = evaluateDatabase migration
