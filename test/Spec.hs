
module Main where

import           Test.Hspec                 (around, hspec)
import           Tests.BeamQueries

import           AppConfig                  as AC
import           Control.Exception          (bracket)
import           Data.ByteString
import           Data.Int
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple
import           Migrate

-- dbFunc = withDatabaseDebug putStrLn

-- INTERESTING NOTE ON MIGRATION
-- receive this error if the tables already exist (not in tests anymore since delete them beforehand)
--  uncaught exception: ErrorCall (Data.Either.Combinators.fromRight: Argument takes form 'Left _'
--  CallStack (from HasCallStack):
--    error, called at src/Data/Either/Combinators.hs:106:24 in either-4.4.1.1-6PiwKYkn4v6B4KO2R2Fu1b:Data.Either.Combinators)

-- drop all tables created by migration. Equivalent to, at the time of writing;
-- execute_ conn "DROP TABLE IF EXISTS users, keys, businesses, contacts, labels, what_labels, items, transformations, locations, events, whats, \"bizTransactions\", whys, wheres, whens, \"labelEvents\", \"userEvents\", hashes, blockchain;"
dropTables :: Connection -> IO Int64
dropTables conn =
  --https://stackoverflow.com/questions/3327312/drop-all-tables-in-postgresql
  execute_ conn "DO $$ DECLARE                                                                              \
               \     r RECORD;                                                                              \
               \ BEGIN                                                                                      \
               \     FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = current_schema()) LOOP    \
               \         EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE';         \
               \     END LOOP;                                                                              \
               \ END $$;                                                                                    "

openConnection :: IO (Connection, Env)
openConnection = do
  conn <- connectPostgreSQL testDbConnStr
  dropTables conn -- drop tables before so if already exist no problems... means tables get overwritten though
  let envT = AC.mkEnvType True
      env  = AC.Env envT conn
  tryCreateSchema True conn
  return (conn, env)

closeConnection :: (Connection, Env) -> IO ()
closeConnection (conn, env) = do
  close conn

withDatabaseConnection :: ((Connection, Env) -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = hspec $ around withDatabaseConnection testQueries
