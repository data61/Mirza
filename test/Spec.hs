{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Tests.BeamQueries
import           Test.Hspec   (hspec, around)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Data.ByteString
import Control.Exception (bracket)
import Migrate

-- dbFunc = withDatabaseDebug putStrLn

-- in following notes, replace per276 with your username
-- initially get into postgres using: sudo -u postgres psql postgres
-- from then on use: sudo -u per276 psql testsupplychainserver2
-- create role per276 with login;
-- create database testsupplychainserver2;
dbConnStr :: ByteString
dbConnStr = "dbname=testsupplychainserver2"

openConnection :: IO Connection
openConnection = do
  conn <- connectPostgreSQL dbConnStr
  migrate dbConnStr
  return conn

closeConnection :: Connection -> IO ()
closeConnection conn = do
  execute_ conn "DROP TABLE IF EXISTS \"bizTransactions\", businesses, contacts, items, events, keys, \"labelEvents\", labels, locations, transformations, users, whats, whens, wheres, whys;"
  close conn

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = hspec $ around withDatabaseConnection testNewUser