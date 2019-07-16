{-# LANGUAGE TypeApplications #-}


module Main where

import           Mirza.Common.Tests.InitClient      (testDbConnectionStringOR,
                                                     testDbNameOR)
import           Mirza.Common.Tests.Utils

import           Mirza.OrgRegistry.Database.Migrate
import           Mirza.OrgRegistry.Database.Schema
import           Mirza.OrgRegistry.Main             hiding (main)
import           Mirza.OrgRegistry.Types            as ORT

import           Test.Hspec.Core.Spec               (sequential)
import           Test.Tasty                         hiding (withResource)
import           Test.Tasty.Hspec                   (around, testSpec)
import           Test.Tasty.Runners                 (NumThreads (..))

import           Mirza.OrgRegistry.Tests.Client
import           Mirza.OrgRegistry.Tests.Keys       (testKeyQueries)
import           Mirza.OrgRegistry.Tests.Org        (testOrgQueries)

import           Control.Exception                  (bracket)
import           Control.Monad.Except               (runExceptT)
import           Database.Beam.Postgres

import           Data.Pool                          (withResource)
import qualified Data.Pool                          as Pool

import           Katip                              (Severity (DebugS))
import           System.IO.Temp                     (emptySystemTempFile)


defaultPool :: IO (Pool.Pool Connection)
defaultPool = Pool.createPool (connectPostgreSQL connectionString) close
                1 -- Number of "sub-pools",
                60 -- How long in seconds to keep a connection open for reuse
                10 -- Max number of connections to have open at any one time
                where
              connectionString = getDatabaseConnectionString testDbConnectionStringOR



openConnection :: IO ORContextMinimal
openConnection = do
  connpool <- defaultPool
  _ <- withResource connpool $ dropTables orgRegistryDB -- drop tables before so if already exist no problems... means tables get overwritten though
  tempFile <- emptySystemTempFile "orgRegistryTests.log"
  let connectionString = getDatabaseConnectionString testDbConnectionStringOR
  ctx <- initORContext (ServerOptionsOR connectionString DebugS (Just tempFile) Dev)
  initRes <- runMigrationWithConfirmation ctx (const (pure Execute))
  case initRes of
    Left err -> print @SqlError err >> error "Database initialisation failed"
    Right () -> pure ctx


closeConnection :: ORContextMinimal -> IO ()
closeConnection = Pool.destroyAllResources . ORT._orDbConnPool

withDatabaseConnection :: (ORContextMinimal -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = do
  either (error . show) pure =<< (liftIO $ runExceptT $ makeDatabase testDbNameOR)

  keyTests <- testSpec "HSpec" (sequential $ around withDatabaseConnection testKeyQueries)
  orgTests <- testSpec "HSpec" (sequential $ around withDatabaseConnection testOrgQueries)
  clientTests <- clientSpec

  defaultMain $ localOption (NumThreads 1) $ testGroup "tests"
    [ keyTests
    , orgTests
    , clientTests
    ]
