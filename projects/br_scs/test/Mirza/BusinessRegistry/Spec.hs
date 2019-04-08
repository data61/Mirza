{-# LANGUAGE TypeApplications #-}


module Main where

import           Mirza.Common.Tests.InitClient           (testDbConnectionStringBR,
                                                          testDbNameBR)
import           Mirza.Common.Tests.Utils

import           Mirza.BusinessRegistry.Database.Migrate
import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.Main             hiding (main)
import           Mirza.BusinessRegistry.Types            as BRT

import           Test.Hspec.Core.Spec                    (sequential)
import           Test.Tasty                              hiding (withResource)
import           Test.Tasty.Hspec                        (around, testSpec)
import           Test.Tasty.Runners                      (NumThreads (..))

import           Mirza.BusinessRegistry.Tests.Business   (testBizQueries)
-- import           Mirza.BusinessRegistry.Tests.Client
import           Mirza.BusinessRegistry.Tests.Keys       (testKeyQueries)

import           Control.Exception                       (bracket)
import           Control.Monad.Except                    (runExceptT)
import           Database.Beam.Postgres

import           Data.Pool                               (withResource)
import qualified Data.Pool                               as Pool

import           Katip                                   (Severity (DebugS))
import           System.IO.Temp                          (emptySystemTempFile)


defaultPool :: IO (Pool.Pool Connection)
defaultPool = Pool.createPool (connectPostgreSQL connectionString) close
                1 -- Number of "sub-pools",
                60 -- How long in seconds to keep a connection open for reuse
                10 -- Max number of connections to have open at any one time
                where
              connectionString = getDatabaseConnectionString testDbConnectionStringBR



openConnection :: IO BRContext
openConnection = do
  connpool <- defaultPool
  _ <- withResource connpool $ dropTables businessRegistryDB -- drop tables before so if already exist no problems... means tables get overwritten though
  tempFile <- emptySystemTempFile "businessRegistryTests.log"
  let connectionString = getDatabaseConnectionString testDbConnectionStringBR
  ctx <- initBRContext (ServerOptionsBR connectionString 16 10 4 DebugS (Just tempFile) Dev)
  initRes <- runMigrationWithConfirmation ctx (const (pure Execute))
  case initRes of
    Left err -> print @SqlError err >> error "Database initialisation failed"
    Right () -> pure ctx


closeConnection :: BRContext -> IO ()
closeConnection = Pool.destroyAllResources . BRT._brDbConnPool

withDatabaseConnection :: (BRContext -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = do
  either (error . show) pure =<< (liftIO $ runExceptT $ makeDatabase testDbNameBR)

  keyTests <- testSpec "HSpec" (sequential $ around withDatabaseConnection testKeyQueries)
  bizTests <- testSpec "HSpec" (sequential $ around withDatabaseConnection testBizQueries)
  --clientTests <- clientSpec TODO: Reinclude when tests are updated to use OAuth rather then basic auth.

  defaultMain $ localOption (NumThreads 1) $ testGroup "tests"
    [ keyTests
    , bizTests
    -- , clientTests  TODO: Reinclude when tests are updated to use OAuth rather then basic auth.
    ]
