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

-- dbFunc = withDatabaseDebug putStrLn

-- in following notes, replace per276 with your username
-- initially get into postgres using: sudo -u postgres psql postgres
-- from then on use: sudo -u per276 psql testsupplychainserver2
-- create role per276 with login;
-- create database testsupplychainserver2;
dbConnStr :: ByteString
dbConnStr = "dbname=testsupplychainserver2"

openConnection :: IO (Connection, Env)
openConnection = do
  conn <- connectPostgreSQL dbConnStr
  let envT = AC.mkEnvType True
      env  = AC.Env envT conn
  tryCreateSchema conn
  return (conn, env)

closeConnection :: (Connection, Env) -> IO ()
closeConnection (conn, env) = do
  -- drop all tables created by migration
  -- TODO = ensure that this list of table names is complete
  execute_ conn "DROP TABLE IF EXISTS users, keys, businesses, contacts, labels, what_labels, items, transformations, locations, events, whats, \"bizTransactions\", whys, wheres, whens, \"labelEvents\", \"userEvents\", hashes, blockchain;"
  close conn

withDatabaseConnection :: ((Connection, Env) -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = hspec $ around withDatabaseConnection testNewUser