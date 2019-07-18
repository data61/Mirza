{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- This module runs database migrations for our application.

module Mirza.SupplyChain.Database.Migrate where

import           Mirza.SupplyChain.Database.Schema.V0001 (migration)
import           Mirza.SupplyChain.Database.Schema.SQL.V0001 ( m_0001 )

import           Mirza.Common.Database                   (runMigrationSimple)
import           Mirza.Common.Types
import           Mirza.Common.Utils
import           Mirza.SupplyChain.Database.Schema       (supplyChainDb)

import qualified Control.Exception                       as E
import           Control.Monad                           (void)

import           System.Exit                             (exitFailure)
import           System.IO                               (hPutStrLn, stderr)

import           Database.Beam                           ()
import           Database.Beam.Backend                   (runNoReturn)
import           Database.Beam.Migrate.Types             (executeMigration)
import           Database.Beam.Postgres                  (Connection, Pg,
                                                          runBeamPostgres,
                                                          runBeamPostgresDebug)
import           Database.PostgreSQL.Simple              (SqlError)



runMigrationWithTriggers :: (Member context '[HasLogging, HasDB]
                            ,Member err     '[AsSqlError])
                        => Connection -> context -> IO (Either err ())
runMigrationWithTriggers conn context = do
  tryCreateSchema False conn
  runAppM context $ runDb $ addLastUpdateTriggers supplyChainDb

-- | Whether or not to run silently
dbMigrationFunc :: Bool -> Connection -> Pg a -> IO a
dbMigrationFunc False = runBeamPostgresDebug putStrLn
dbMigrationFunc _     = runBeamPostgres

createSchema :: Bool -> Connection -> IO ()
createSchema runSilently conn =
  void $ dbMigrationFunc runSilently conn $ executeMigration runNoReturn (migration ())

tryCreateSchema :: Bool -> Connection -> IO ()
tryCreateSchema runSilently conn = E.catch (createSchema runSilently conn) handleErr
  where
    handleErr :: SqlError -> IO ()
    handleErr err = do
      hPutStrLn stderr $ "Migration failed with error:  " <> show err
      exitFailure

migrate :: ( Member context '[HasLogging, HasDB] ) => context -> IO (Either SqlError ())
migrate ctx = runMigrationSimple ctx migrations

  where
    migrations = [ m_0001
                 ]
