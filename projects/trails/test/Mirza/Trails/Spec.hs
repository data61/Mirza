{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Main where


import           Mirza.Trails.Database.Migrate
import           Mirza.Trails.Types

import           Mirza.Trails.Tests.Client
import           Mirza.Trails.Tests.InitClient
import           Mirza.Trails.Tests.Types

import           Mirza.Common.Tests.Utils

import           Test.Tasty                    hiding (withResource)
import           Test.Tasty.Runners            (NumThreads (..))

import           Control.Monad.Except          (liftIO, runExceptT)


main :: IO ()
main = do
  either (error . show) pure =<< (liftIO $ runExceptT $ makeDatabase testDbNameTrails)

  context <- makeTrailsTestContext
  initialisationResult <- runMigrationSimple context migrations
  either (\err -> print @TrailsServiceError err >> error "Failed initalising database.") (\_ -> pure()) initialisationResult

  clientTests <- clientSpec

  defaultMain $ localOption (NumThreads 1) $ testGroup "tests"
    [ clientTests
    ]
