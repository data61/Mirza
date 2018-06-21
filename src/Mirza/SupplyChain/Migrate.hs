
-- | Module containing functions to run the migration function
module Mirza.SupplyChain.Migrate
  (
    defaultDbConnectionStr
  , migrate
  , tryCreateSchema
  ) where

import           Mirza.SupplyChain.MigrateScript (migrationStorage)

import qualified Control.Exception               as E
import           Control.Monad                   (void)
import           Data.ByteString.Char8           (ByteString)
import           Database.Beam                   (withDatabase,
                                                  withDatabaseDebug)
import           Database.Beam.Backend           (runNoReturn)
import           Database.Beam.Migrate.Types     (executeMigration)
import           Database.Beam.Postgres          (Connection, Pg)
import           Database.PostgreSQL.Simple      (SqlError, connectPostgreSQL)

-- | Whether or not to run silently
dbMigrationFunc :: Bool -> Connection -> Pg a -> IO a
dbMigrationFunc False = withDatabaseDebug putStrLn
dbMigrationFunc _     = withDatabase

-- | Default connection string. Be careful with this construct as it could lead to
-- problems...users not specifying the database and accidentally operating on
-- the wrong database.
defaultDbConnectionStr :: ByteString
defaultDbConnectionStr = "dbname=devsupplychainserver"

createSchema :: Bool -> Connection -> IO ()
createSchema runSilently conn = do
  void $ dbMigrationFunc runSilently conn $ executeMigration runNoReturn migrationStorage

-- dropSchema :: Connection -> IO ()
-- dropSchema conn = do
--   dbFunc conn $ executeMigration runNoReturn dropTables
--   return ()

tryCreateSchema :: Bool -> Connection -> IO ()
tryCreateSchema runSilently conn = E.catch (createSchema runSilently conn) handleErr
  where
    handleErr :: SqlError -> IO ()
    handleErr  str = putStrLn $ "XXXXXX " ++ show str ++ " XXXXXX"

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
