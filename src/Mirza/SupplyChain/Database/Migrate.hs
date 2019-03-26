{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- This module runs database migrations for our application.

module Mirza.SupplyChain.Database.Migrate where

import           Mirza.SupplyChain.Database.Schema.V0001 (migration)

import           Mirza.Common.Types
import           Mirza.Common.Utils
import           Mirza.SupplyChain.Database.Schema       (supplyChainDb)

import qualified Control.Exception                       as E
import           Control.Monad                           (void)

import           Data.ByteString.Char8                   (ByteString)
import           Database.Beam                           (withDatabase,
                                                          withDatabaseDebug)
import           Database.Beam.Backend                   (runNoReturn)
import           Database.Beam.Migrate.Types             (executeMigration)
import           Database.Beam.Postgres                  (Connection, Pg)
import           Database.PostgreSQL.Simple              (SqlError,
                                                          connectPostgreSQL)



runMigrationWithTriggers :: (Member context '[HasLogging, HasDB]
                            ,Member err     '[AsSqlError])
                        => Connection -> context -> IO (Either err ())
runMigrationWithTriggers conn context = do
  tryCreateSchema False conn
  runAppM context $ runDb $ addLastUpdateTriggers supplyChainDb

-- | Whether or not to run silently
dbMigrationFunc :: Bool -> Connection -> Pg a -> IO a
dbMigrationFunc False = withDatabaseDebug putStrLn
dbMigrationFunc _     = withDatabase

createSchema :: Bool -> Connection -> IO ()
createSchema runSilently conn =
  void $ dbMigrationFunc runSilently conn $ executeMigration runNoReturn (migration ())

tryCreateSchema :: Bool -> Connection -> IO ()
tryCreateSchema runSilently conn = E.catch (createSchema runSilently conn) handleErr
  where
    handleErr :: SqlError -> IO ()
    handleErr err = putStrLn $ "XXXXXX " <> show err <> " XXXXXX"

migrate :: (Member context '[HasLogging, HasDB])
        => context -> ByteString -> IO ()
migrate ctx connStr = do
  conn <- connectPostgreSQL connStr
  r <- runMigrationWithTriggers conn ctx
  case r of
    Left (err :: SqlError) -> print $ "Table could not be created. Error:  " <> show err
    Right _succ -> print $ "Successfully created table with conn: " <> show connStr

