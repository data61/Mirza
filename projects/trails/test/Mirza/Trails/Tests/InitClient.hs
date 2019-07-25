module Mirza.Trails.Tests.InitClient where


import           Mirza.Trails.Tests.Types

import           Mirza.Trails.Main               as TrailsMain

import           Mirza.Common.Tests.ServantUtils
import           Mirza.Common.Tests.Utils        (getDatabaseConnectionString)

import           Mirza.Common.Types

import           Katip                           (Severity (DebugS))

import           Servant.Client                  (BaseUrl (..))

import           System.IO.Temp                  (emptySystemTempFile)

import           Control.Concurrent              (ThreadId)


trailsTestOptions :: Maybe FilePath -> ServerOptionsTrails
trailsTestOptions maybeFilepath = ServerOptionsTrails connectionString DebugS maybeFilepath Dev
  where
    connectionString = getDatabaseConnectionString testDbConnectionStringTrails


runTrailsApp :: IO (ThreadId, BaseUrl)
runTrailsApp = do
  tempFile <- emptySystemTempFile "trailsServiceTests.log"
  let currentTrailsOptions = trailsTestOptions (Just tempFile)
  context <- initTrailsContext currentTrailsOptions

  -- let TrailsDB trailsTable = trailsDB

  -- flushDbResult <- runAppM @_ @TrailsError context $ runDb $ do
  --     let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
  --     deleteTable trailsTable
  -- flushDbResult `shouldSatisfy` isRight

  (tid,orul) <- startWaiApp =<< TrailsMain.initApplication context
  pure (tid, orul)
