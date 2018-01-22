{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Tests.BeamQueries
import           Test.Hspec   (hspec, around)

import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Data.ByteString
import Control.Exception (bracket)

-- dbFunc = withDatabaseDebug putStrLn

dbConnStr :: ByteString
dbConnStr = "dbname=testsupplychainserver"

openConnection :: IO Connection
openConnection = connectPostgreSQL dbConnStr

closeConnection :: Connection -> IO ()
closeConnection = close

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = hspec $ around withDatabaseConnection testNewUser