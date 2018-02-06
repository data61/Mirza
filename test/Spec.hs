{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Tests.BeamQueries
import           Test.Hspec   (hspec, around)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Data.ByteString
import Control.Exception (bracket)
import Migrate
import AppConfig as AC
import Data.Int

-- dbFunc = withDatabaseDebug putStrLn

-- in following notes, replace per276 with your username
-- initially get into postgres using: sudo -u postgres psql postgres
-- from then on use: sudo -u per276 psql testsupplychainserver2
-- create role per276 with login;
-- create database testsupplychainserver;
dbConnStr :: ByteString
dbConnStr = "dbname=testsupplychainserver"

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
-- following is single line version in case want to for style, or want to copy into terminal... is same query
-- execute_ conn "DO $$ DECLARE r RECORD; BEGIN FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = current_schema()) LOOP EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE'; END LOOP; END $$;"

openConnection :: IO (Connection, Env)
openConnection = do
  conn <- connectPostgreSQL dbConnStr                                                                                  
  dropTables conn -- drop tables before so if already exist no problems... means tables get overwritten though
  let envT = AC.mkEnvType True
      env  = AC.Env envT conn
  tryCreateSchema conn
  return (conn, env)

closeConnection :: (Connection, Env) -> IO ()
closeConnection (conn, env) = do
  close conn

withDatabaseConnection :: ((Connection, Env) -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = hspec $ around withDatabaseConnection testNewUser