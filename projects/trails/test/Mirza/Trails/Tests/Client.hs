{-# LANGUAGE OverloadedStrings #-}


module Mirza.Trails.Tests.Client where


import           Mirza.Trails.Tests.InitClient

import           Mirza.Trails.Client.Servant
import           Mirza.Trails.Types

import           Mirza.Common.Tests.ServantUtils

import           Mirza.Common.Tests.Utils        (checkFailureMessage,
                                                  checkFailureStatus,
                                                  within1Second)

import           Mirza.Common.Types

import           Data.GS1.EPC                    (GS1CompanyPrefix (..))
import           Data.GS1.EventId                (EventId (..))

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Network.HTTP.Types.Status       as NS

import           Servant.API.ContentTypes

import           Control.Exception               (bracket)
import           Data.Either                     (isLeft, isRight)
import           Data.Time.Clock
import           Data.UUID.V4


-- === Trails Servant Client tests
clientSpec :: IO TestTree
clientSpec = do
  let todoTests = testCaseSteps "TODO" $ \step ->
        bracket runTrailsApp (\(a,b) -> endWaiApp (a,b)) $ \(_tid, baseurl) -> do
          let http = runClient baseurl

          step "That when there are no entries in the database that getting by signature responds corretly."
          getGetSignatureInitialEmpty <- http $ getTrailBySignature (SignaturePlaceholder "")
          getGetSignatureInitialEmpty `shouldBe` Right []

          step "That when there are no entries in the database that getting by eventId responds corretly."
          empty_non_matching_uuid <- liftIO nextRandom
          getGetEventIdInitialEmpty <- http $ getTrailByEventId (EventId empty_non_matching_uuid)
          getGetEventIdInitialEmpty `shouldSatisfy` isLeft
          getGetEventIdInitialEmpty `shouldSatisfy` (checkFailureStatus NS.notFound404)
          getGetEventIdInitialEmpty `shouldSatisfy` (checkFailureMessage "A trail with the matching EventId was not found.")

          step "That adding the first entry in a trail works."
          singleEntry <- buildEntry
          addFirstEntryResult <- http $ addTrail [singleEntry]
          addFirstEntryResult `shouldBe` Right NoContent

          step "That getting a single entry trail works."
          getSingleEntryResult <- http $ getTrailBySignature (trailEntrySignature singleEntry)
          getSingleEntryResult `shouldMatchTrail` [singleEntry]



  -- Test trail head node.
  -- Test forked trail (1 into 2).
  -- Test joined trail (2 into 1).
  -- Test that invalid signature fails.
  -- Test that invalid version fails.
  -- Test that timestamp in the future is invalid.
  -- Test when trying to add an entry where the parent isn't stored in the service fails.
  -- That if the entry version is not 1 adding fails.
-- Test adding a trail with an entry that already has a signature in the db fails.
-- Test that when a trail that has multiple entries fails that the other entries are not added.

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
        , todoTests
        ]


buildEntry :: IO TrailEntry
buildEntry = do
  time <- liftIO getCurrentTime
  uuid <- liftIO nextRandom
  pure $ TrailEntry 1
                  (EntryTime time)
                  (GS1CompanyPrefix "0000001")
                  (EventId uuid)
                  []
                  (SignaturePlaceholder "TODO")


shouldMatchTrail :: (Show a, Eq a) => Either a [TrailEntry] -> [TrailEntry] -> Expectation
shouldMatchTrail actual@(Left _) _       = actual `shouldSatisfy` isRight
shouldMatchTrail (Right actual) expected = do
  (length actual) `shouldBe` (length expected)
  _ <- sequence $ zipWith shouldMatchEntry actual expected  -- Note: This is overly constrained, but while it works this seems find and can always update this check if in future we have elements that are in different order.
  pure $ ()

shouldMatchEntry :: TrailEntry -> TrailEntry -> Expectation
shouldMatchEntry (TrailEntry   actual_version (EntryTime   actual_timestamp)   actual_gs1_company_prefix   actual_eventId   actual_previous_signatures   actual_signature)
      (TrailEntry expected_version (EntryTime expected_timestamp) expected_gs1_company_prefix expected_eventId expected_previous_signatures expected_signature) = do
        actual_version `shouldBe` expected_version
        actual_timestamp `shouldSatisfy` within1Second expected_timestamp
        actual_gs1_company_prefix `shouldBe` expected_gs1_company_prefix
        actual_eventId `shouldBe` expected_eventId
        actual_previous_signatures `shouldBe` expected_previous_signatures -- Note: This is overly constrained, but while it works this seems find and can always update this check if in future we have elements that are in different order.
        actual_signature `shouldBe` expected_signature



