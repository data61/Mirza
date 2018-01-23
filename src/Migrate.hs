{-# LANGUAGE OverloadedStrings #-}
module Migrate where

import qualified Control.Exception as E
import           StorageBeam -- the schemas
import           Database.Beam (withDatabaseDebug)
import           Database.Beam.Postgres (Connection, Pg)
import           Database.Beam.Migrate.Types
import           Database.Beam.Backend (runNoReturn)

import           Database.PostgreSQL.Simple(SqlError ,connectPostgreSQL)
import           Data.ByteString.Char8 (ByteString, pack)


dbFunc :: Connection -> Pg a -> IO a
dbFunc = withDatabaseDebug putStrLn

connectionStr :: ByteString
connectionStr = pack "dbname=testsupplychainserver"

createSchema :: Connection -> IO ()
createSchema conn = do
  dbFunc conn $ executeMigration runNoReturn migrationStorage
  return ()

tryCreateSchema :: Connection -> IO ()
tryCreateSchema conn = E.catch (createSchema conn) handleErr
  where
    handleErr :: SqlError -> IO ()
    handleErr e = print e

migrate :: IO ()
migrate = do
  conn <- connectPostgreSQL connectionStr
  tryCreateSchema conn
  print $ "Successfully created table. ConnectionStr was " ++ show connectionStr
