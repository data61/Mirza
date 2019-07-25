{-# LANGUAGE OverloadedStrings #-}


module Mirza.Trails.Tests.Types where


import           Mirza.Common.Tests.Utils


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Default database name when running tests for the Trials Service. Be careful
-- using this construct as it could lead to problems...users not specifying the
-- database and accidentally operating on the wrong database.
testDbNameTrails :: DatabaseName
testDbNameTrails = DatabaseName "testtrails"


-- | Default database connection string used when running tests for the Trails
-- Service. Be careful using this construct as it could lead to problems...users
-- not specifying the database and accidentally operating on the wrong database.
testDbConnectionStringTrails :: DatabaseConnectionString
testDbConnectionStringTrails = databaseNameToConnectionString testDbNameTrails
