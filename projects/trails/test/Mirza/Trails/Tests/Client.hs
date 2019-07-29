module Mirza.Trails.Tests.Client where


import           Mirza.Trails.Tests.InitClient

import           Mirza.Trails.Client.Servant

import           Mirza.Common.Tests.ServantUtils

import           Mirza.Common.Types

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Exception               (bracket)
import           Data.Either                     (isRight)


-- === Trails Servant Client tests
clientSpec :: IO TestTree
clientSpec = do
  let healthTests = testCaseSteps "Provides health status" $ \step ->
        bracket runTrailsApp (\(a,b) -> endWaiApp (a,b)) $ \(_tid, baseurl) -> do
          let http = runClient baseurl

          step "Status results in 200"
          healthResult <- http health
          healthResult `shouldSatisfy` isRight
          healthResult `shouldBe` (Right HealthResponse)


  pure $ testGroup "Trails HTTP Client tests"
        [
          healthTests
        ]
