
module Main where

import           Mirza.SupplyChain.Database.Migrate
import           Mirza.SupplyChain.Database.Schema
import           Mirza.SupplyChain.Main             hiding (main)
import           Mirza.SupplyChain.Types            as ST

import           Test.Hspec.Core.Spec               (sequential)
import           Test.Tasty                         hiding (withResource)
import           Test.Tasty.Hspec                   (around, testSpec)
import           Test.Tasty.Runners                 (NumThreads (..))

import           Mirza.Common.Tests.InitClient      (testDbConnectionStringSCS,
                                                     testDbNameSCS)
import           Mirza.Common.Tests.Utils

import           Mirza.SupplyChain.Tests.Citrus     (citrusSpec)
import           Mirza.SupplyChain.Tests.Client
import           Mirza.SupplyChain.Tests.Service    (testServiceQueries)

import           Control.Exception                  (bracket)
import           Control.Monad.Except               (runExceptT)
import           Database.Beam.Postgres

import           Data.Pool                          (Pool, destroyAllResources,
                                                     withResource)
import qualified Data.Pool                          as Pool

import           Katip                              (Severity (DebugS))
import           System.IO.Temp                     (emptySystemTempFile)


defaultPool :: IO (Pool Connection)
defaultPool = Pool.createPool (connectPostgreSQL connectionString) close
                1 -- Number of "sub-pools",
                60 -- How long in seconds to keep a connection open for reuse
                10 -- Max number of connections to have open at any one time
                where
              connectionString = getDatabaseConnectionString testDbConnectionStringSCS


openConnection :: IO SCSContext
openConnection = do
  tempFile <- emptySystemTempFile "supplyChainServerTests.log"
  connpool <- defaultPool
  _ <- withResource connpool $ dropTables supplyChainDb -- drop tables before so if already exist no problems... means tables get overwritten though
  withResource connpool (tryCreateSchema True)
  let connectionString = getDatabaseConnectionString testDbConnectionStringSCS
  initSCSContext (ServerOptionsSCS Dev False connectionString 8000 14 8 1 DebugS
                                (Just ("127.0.0.1", 8200)) (Just tempFile))

closeConnection :: SCSContext -> IO ()
closeConnection = destroyAllResources . ST._scsDbConnPool

withDatabaseConnection :: (SCSContext -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = do
  either (error . show) pure =<< liftIO (runExceptT $ makeDatabase testDbNameSCS)

  serviceTests <- testSpec "HSpec" (sequential $ around withDatabaseConnection testServiceQueries)
  clientTests <- clientSpec
  citrusTests <- citrusSpec
  defaultMain $ localOption (NumThreads 1) $ testGroup "SCS tests"
    [ serviceTests
    , clientTests
    , citrusTests
    ]
