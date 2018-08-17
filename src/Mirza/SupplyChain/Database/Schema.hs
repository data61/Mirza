module Mirza.SupplyChain.Database.Schema
  ( module Current
  , migration
  , supplyChainDb
  , checkedSupplyChainDb
  , primaryKey
  ) where

import           Database.Beam                           (DatabaseSettings)
import           Database.Beam                           as B
import           Database.Beam.Migrate.Types             hiding (migrateScript)
import           Database.Beam.Postgres                  (PgCommandSyntax,
                                                          Postgres)
import           Database.Beam.Schema.Tables             (primaryKey)

import           Mirza.SupplyChain.Database.Schema.V0001 as Current hiding
                                                                     (migration)

import qualified Mirza.SupplyChain.Database.Schema.V0001 as V0001


migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres Current.SupplyChainDb)
migration = migrationStep "Initial commit" V0001.migration

checkedSupplyChainDb :: CheckedDatabaseSettings Postgres Current.SupplyChainDb
checkedSupplyChainDb = evaluateDatabase migration

supplyChainDb :: DatabaseSettings Postgres SupplyChainDb
supplyChainDb = unCheckDatabase checkedSupplyChainDb
