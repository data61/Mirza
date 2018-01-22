{-# LANGUAGE OverloadedStrings #-}
module AppConfig (
    dbFunc
)
where

import           Database.PostgreSQL.Simple (Connection)
import qualified Database.Beam as B
import           Database.Beam.Postgres (Pg)

data EnvType = Prod | Dev

-- | Given the environment, returns which db function to use
-- dbFunc :: EnvType -> (Connection -> Pg a0 -> IO a0)
-- dbFunc Prod = withDatabase
-- dbFunc _    = withDatabaseDebug putStrLn

-- |for the moment, comment in/out the appropriate line to the get the proper
-- function
dbFunc :: Connection -> (Pg a0 -> IO a0)
-- dbFunc = withDatabase
dbFunc = B.withDatabaseDebug putStrLn
