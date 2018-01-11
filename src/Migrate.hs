{-# LANGUAGE OverloadedStrings #-}
module Migrate where

import StorageBeam -- the schemas
import Database.Beam (withDatabaseDebug)
import Database.Beam.Postgres (Connection)
import Database.Beam.Migrate.Types
import Database.Beam.Backend (runNoReturn)


import qualified Control.Exception as E
import Database.PostgreSQL.Simple(SqlError ,connectPostgreSQL)
import Database.Beam.Postgres (Connection)
import Data.ByteString.Char8 (ByteString, pack)

-- import qualified Persistence.PaddockPersist as PP
-- import Persistence.Schema.Migration (createSchema)
-- import Persistence.Types (PaddockUpdate(..), PaddockInsert(..))

connectionStr :: ByteString
connectionStr = pack "dbname=testsupplychainserver"

createSchema :: Connection -> IO ()
createSchema conn = do
  withDatabaseDebug putStrLn conn $ executeMigration runNoReturn migrationStorage
  return ()


tryCreateSchema :: Connection -> IO ()
tryCreateSchema conn = E.catch (createSchema conn) handleErr
  where
    handleErr :: SqlError -> IO ()
    handleErr e = putStrLn "Schema up to date"


migrate :: IO ()
migrate = do
  conn <- connectPostgreSQL connectionStr
  tryCreateSchema conn
--   PP.insertPaddock conn (PaddockInsert (Just "Thanh's Paddock 2") (-33.8046874) 151.2708806 "Barley" "Wheat" "thanh")
--   PP.updatePaddock conn "thanh" (PaddockUpdate (PP.toPaddockId 1) (Just "Thanh's Paddock 1") Nothing Nothing)
--   removedPP <- PP.removePaddockUser conn "thanh" (PP.toPaddockId 8)
--   paddocks <- PP.selectPaddocks conn "thanh"
  print $ "Successfully created table. ConnectionStr was " ++ show connectionStr


