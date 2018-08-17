{-# LANGUAGE TypeApplications #-}

-- This module runs database migrations for our application.

module Mirza.SupplyChain.Database.Migrate where

import           Mirza.SupplyChain.Database.Schema.V0001 (migration)

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
    handleErr  str = putStrLn $ "XXXXXX " ++ show str ++ " XXXXXX"

migrate :: ByteString -> IO ()
migrate connStr = do
  conn <- connectPostgreSQL connStr
  tryCreateSchema False conn
  print $ "Successfully created table. ConnectionStr was " ++ show connStr
