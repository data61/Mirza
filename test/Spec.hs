{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Tests.BeamQueries
import           Test.Hspec   (hspec, around)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Data.ByteString
import Control.Exception (bracket)

-- dbFunc = withDatabaseDebug putStrLn

-- initially get into postgres using: sudo -u postgres psql postgres
-- from then on use: sudo -u per276 psql testsupplychainserver2
-- create role per276 with login;
-- create database testsupplychainserver2;
dbConnStr :: ByteString
dbConnStr = "dbname=testsupplychainserver2"

openConnection :: IO Connection
openConnection = connectPostgreSQL dbConnStr

closeConnection :: Connection -> IO ()
closeConnection = close

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = hspec $ around withDatabaseConnection testNewUser