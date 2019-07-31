{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mirza.Trails.Tests.InitClient where


import           Mirza.Trails.Tests.Types

import           Mirza.Trails.Database.Schema
import           Mirza.Trails.Main               as TrailsMain
import           Mirza.Trails.Types

import           Mirza.Common.Tests.ServantUtils
import           Mirza.Common.Tests.Utils        (getDatabaseConnectionString)

import           Mirza.Common.Types

import           Test.Hspec.Expectations

import           Katip                           (Severity (DebugS))

import           Database.Beam.Query             (delete, runDelete, val_)

import           Servant.Client                  (BaseUrl (..))

import           System.IO.Temp                  (emptySystemTempFile)

import           Control.Concurrent              (ThreadId)

import           Data.Either                     (isRight)


trailsTestOptions :: Maybe FilePath -> ServerOptionsTrails
trailsTestOptions maybeFilepath = ServerOptionsTrails connectionString DebugS maybeFilepath Dev
  where
    connectionString = getDatabaseConnectionString testDbConnectionStringTrails


runTrailsApp :: IO (ThreadId, BaseUrl)
runTrailsApp = do
  tempFile <- emptySystemTempFile "trailsServiceTests.log"
  let currentTrailsOptions = trailsTestOptions (Just tempFile)
  context <- initTrailsContext currentTrailsOptions

  let TrailsDB entriesTable previousTrable = trailsDB

  flushDbResult <- runAppM @_ @TrailsServiceError context $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable entriesTable
      deleteTable previousTrable
  flushDbResult `shouldSatisfy` isRight

  (tid,orul) <- startWaiApp =<< TrailsMain.initApplication context
  pure (tid, orul)
