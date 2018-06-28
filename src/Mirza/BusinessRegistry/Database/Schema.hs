{-# LANGUAGE TypeApplications #-}

module Mirza.BusinessRegistry.Database.Schema
  ( module Current
    , migration
    , runMigrationInteractive
    , businessRegistryDB ) where

-- import           Control.Arrow ((>>>))

import qualified Data.ByteString.Lazy.Char8                   as BSL

import           Control.Monad.IO.Class                       (liftIO)

import           Control.Lens                                 (view, _1)

import           Database.Beam                                (DatabaseSettings)
import           Database.Beam.Backend                        (runNoReturn)
import           Database.Beam.Migrate.Simple                 (runSimpleMigration,
                                                               simpleMigration)
import           Database.Beam.Migrate.Types                  hiding
                                                               (migrateScript)
import           Database.Beam.Postgres                       (Pg,
                                                               PgCommandSyntax,
                                                               Postgres)
import           Database.Beam.Postgres.Migrate               (migrateScript,
                                                               migrationBackend)


import           Mirza.Common.Types

import           Mirza.BusinessRegistry.Database.Schema.V0001 as Current hiding (migration)

import qualified Mirza.BusinessRegistry.Database.Schema.V0001 as V0001 (migration)
-- import qualified Mirza.BusinessRegistry.Database.Schema.V0002 as V0002 (businessRegistryDB, migration)


migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres Current.BusinessRegistryDB)
migration = migrationStep "Initial commit" V0001.migration
           -- >>> migrationStep """todo comment""" V0002.migration

businessRegistryDB :: DatabaseSettings Postgres Current.BusinessRegistryDB
businessRegistryDB = unCheckDatabase checkedBusinessRegistryDB

checkedBusinessRegistryDB :: CheckedDatabaseSettings Postgres Current.BusinessRegistryDB
checkedBusinessRegistryDB = evaluateDatabase migration



-- TODO: Use autoMigrate if possible and confirm with the user whether to do dangerous migrations explicitly
-- TODO: Move this into Mirza.Common.Beam
runMigrationInteractive ::
  (HasKatipLogEnv context
  , HasKatipContext context
  , HasConnPool context
  , HasEnvType context
  , AsSqlError err)
  => context -> IO (Either err ())
runMigrationInteractive context = do
   mapM_ BSL.putStr $ migrateScript migration
   putStrLn "type YES to confirm applying this migration:"
   confirm <- getLine
   case confirm of
    "YES" -> runAppM context $ runDb $ do
      -- simpleMigration - needs Connection, postgresmigrationBackend, checked migration
          conn <- view _1
          mcommands <- liftIO $ simpleMigration migrationBackend conn checkedBusinessRegistryDB
          case mcommands of
            Nothing -> fail "lol"
            Just commands -> do
              -- TODO: Print these commands as they're just the changes needed
              liftIO $ runSimpleMigration
                          @PgCommandSyntax
                          @Postgres
                          @_
                          @Pg
                          conn commands
    _ -> fail "Ok, not migrating"
