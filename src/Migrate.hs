{-# LANGUAGE OverloadedStrings #-}
module Migrate where

import qualified Control.Exception as E
import           StorageBeam -- the schemas
import           Database.Beam (withDatabaseDebug, withDatabase)
import           Database.Beam.Postgres (Connection, Pg)
import           Database.Beam.Migrate.Types (executeMigration)
import           Database.Beam.Backend (runNoReturn)

import           Database.PostgreSQL.Simple(SqlError ,connectPostgreSQL)
import           Data.ByteString.Char8 (ByteString)

-- boolean is whether to run silently
dbMigrationFunc :: Bool -> Connection -> Pg a -> IO a
dbMigrationFunc False = withDatabaseDebug putStrLn
dbMigrationFunc _ = withDatabase

defConnectionStr :: ByteString
defConnectionStr = "dbname=devsupplychainserver"

createSchema :: Bool -> Connection -> IO ()
createSchema runSilently conn = do
  dbMigrationFunc runSilently conn $ executeMigration runNoReturn $ migrationStorage
  return ()

-- dropSchema :: Connection -> IO ()
-- dropSchema conn = do
--   dbFunc conn $ executeMigration runNoReturn dropTables
--   return ()

tryCreateSchema :: Bool -> Connection -> IO ()
tryCreateSchema runSilently conn = E.catch (createSchema runSilently conn) handleErr
  where
    handleErr :: SqlError -> IO ()
    handleErr  = print 

-- tryDrop :: Connection -> IO ()
-- tryDrop conn = E.catch (dropSchema conn) handleErr
--   where
--     handleErr :: SqlError -> IO ()
--     handleErr  = print 

migrate :: ByteString -> IO ()
migrate connStr = do
  conn <- connectPostgreSQL connStr
  tryCreateSchema False conn
  print $ "Successfully created table. ConnectionStr was " ++ show connStr


-- deleteAllTables :: ByteString -> IO ()
-- deleteAllTables connStr = do
--   conn <- connectPostgreSQL connStr
--   tryDrop conn
--   print $ "Dropped all tables. Constr was: " ++ show connStr
