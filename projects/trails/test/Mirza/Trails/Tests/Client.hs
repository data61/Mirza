module Mirza.Trails.Tests.Client where


import           Mirza.Trails.Client.Servant

import           Mirza.Trails.Tests.InitClient

import           Mirza.Common.Tests.ServantUtils

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Exception               (bracket)


-- === OR Servant Client tests
clientSpec :: IO TestTree
clientSpec = do




  let healthTests = testCaseSteps "Provides health status" $ \step ->
        bracket runTrailsApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl) -> do
          let http = runClient baseurl

          step "Status results in 200"
          -- healthResult <- http health
          -- healthResult `shouldSatisfy` isRight
          -- healthResult `shouldBe` (Right HealthResponse)


  pure $ testGroup "Trails HTTP Client tests"
        [
          healthTests
        ]
