{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

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
import           Servant.Client

import           Control.Exception               (bracket)
import           Control.Monad
import           Data.Either                     (isLeft, isRight)
import           Data.List
import           Data.Time.Clock
import           Data.UUID
import           Data.UUID.V4


-- === Trails Servant Client tests
clientSpec :: IO TestTree
clientSpec = do
  let todoTests = testCaseSteps "TODO" $ \step ->
        bracket runTrailsApp (\(a,b) -> endWaiApp (a,b)) $ \(_tid, baseurl) -> do
          let http = runClient baseurl

          -- These test cases should largely be property based tests, but as we currently have no property based testing
          -- machinery and time constraints of the project we just implement a bunch of copy pasted simple tests to
          -- check a variety of classic test boundry scenarios.

          step "That when there are no entries in the database that getting by signature responds corretly."
          getGetSignatureInitialEmpty <- http $ getTrailBySignature (SignaturePlaceholder "invalid")
          getGetSignatureInitialEmpty `shouldSatisfy` isLeft
          getGetSignatureInitialEmpty `shouldSatisfy` (checkFailureStatus NS.notFound404)
          getGetSignatureInitialEmpty `shouldSatisfy` (checkFailureMessage "A trail with a matching signature was not found.")

          step "That when there are no entries in the database that getting by eventId responds corretly."
          empty_non_matching_uuid <- liftIO nextRandom
          getGetEventIdInitialEmpty <- http $ getTrailByEventId (EventId empty_non_matching_uuid)
          getGetEventIdInitialEmpty `shouldSatisfy` isLeft
          getGetEventIdInitialEmpty `shouldSatisfy` (checkFailureStatus NS.notFound404)
          getGetEventIdInitialEmpty `shouldSatisfy` (checkFailureMessage "A trail with the matching EventId was not found.")


          -- Trail: *
          step "That adding the first entry in a trail works."
          singleEntry <- buildEntry
          addFirstEntryResult <- http $ addTrail [singleEntry]
          addFirstEntryResult `shouldBe` Right NoContent

          step "That getting a single entry trail by signature works."
          getSingleEntryBySignatureResult <- http $ getTrailBySignature (trailEntrySignature singleEntry)
          getSingleEntryBySignatureResult `shouldMatchTrail` [singleEntry]

          step "That getting a single entry trail by eventId works."
          getSingleEntryByEventIdResult <- http $ getTrailByEventId (trailEntryEventID singleEntry)
          getSingleEntryByEventIdResult `shouldMatchTrail` [singleEntry]


          let checkTrailWithContext = checkTrail step http

          -- Trail: *---*
          twoEntryTrail <- join $ addPreviousEntry <$> (fmap pure buildEntry)
          checkTrailWithContext "2 Entry Trail" twoEntryTrail


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
-- Test adding a trail with two entries with the same signature (i.e. duplicate entry).

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
  let unsignedEntry = TrailEntry 1
                                 (EntryTime time)
                                 (GS1CompanyPrefix "0000001")
                                 (EventId uuid)
                                 []
                                 (SignaturePlaceholder "")
  pure unsignedEntry{trailEntrySignature = buildSignature unsignedEntry}


-- This is a hack for now untril we implement signatures properly.
buildSignature :: TrailEntry -> SignaturePlaceholder
buildSignature entry = SignaturePlaceholder $ "SignaturePlaceholder-" <> (toText $ unEventId $ trailEntryEventID entry)


-- Note: Adds the new entry to the element at the start of the list and the newly created entry will be prepended to
-- the entry list and leave the remainder of the entry list in the same order.
addPreviousEntry :: [TrailEntry] -> IO [TrailEntry]
addPreviousEntry (entry : _) = do
  newEntry <- buildEntry
  let updatedEntry = addPreviousEntrySignature entry (trailEntrySignature newEntry)
  pure [newEntry, updatedEntry]
-- Could just define the following  as buildEntry, but it seems that this is likely to be a logic error and so its probably better to just fail here.
addPreviousEntry [] = error "Error: There is a logic error in the tests. Can't add a previous entry of a non existant entry."


addNextEntry ::  [TrailEntry] -> IO [TrailEntry]
addNextEntry (entry : _) = do
  newEntry <- buildEntry
  let updatedWithPreviousEntry = addPreviousEntrySignature newEntry (trailEntrySignature entry)
  pure [updatedWithPreviousEntry, entry]
-- Could just define the following  as buildEntry, but it seems that this is likely to be a logic error and so its probably better to just fail here.
addNextEntry [] = error "Error: There is a logic error in the tests. Can't add the next entry of a non existant entry."


addPreviousEntrySignature :: TrailEntry -> SignaturePlaceholder -> TrailEntry
addPreviousEntrySignature entry sig = entry{trailEntryParentSignatures = sig : (trailEntryParentSignatures entry)}


shouldMatchTrail :: (Show a, Eq a) => Either a [TrailEntry] -> [TrailEntry] -> Expectation
shouldMatchTrail actual@(Left _) _       = actual `shouldSatisfy` isRight
shouldMatchTrail (Right actual) expected = do
  (length actual) `shouldBe` (length expected)
  _ <- sequence $ zipWith shouldMatchEntry (sort actual) (sort expected)
  pure $ ()


shouldMatchEntry :: TrailEntry -> TrailEntry -> Expectation
shouldMatchEntry (TrailEntry   actual_version (EntryTime   actual_timestamp)   actual_gs1_company_prefix   actual_eventId   actual_previous_signatures   actual_signature)
      (TrailEntry expected_version (EntryTime expected_timestamp) expected_gs1_company_prefix expected_eventId expected_previous_signatures expected_signature) = do
        actual_version `shouldBe` expected_version
        actual_timestamp `shouldSatisfy` within1Second expected_timestamp
        actual_gs1_company_prefix `shouldBe` expected_gs1_company_prefix
        actual_eventId `shouldBe` expected_eventId
        (sort actual_previous_signatures) `shouldBe` (sort expected_previous_signatures)
        actual_signature `shouldBe` expected_signature


-- Test the 3 end points (addTrail, getTrailBySignature, getTrailByEventId) for the specified trail.
checkTrail :: (String -> IO()) -> (forall a. ClientM a -> IO (Either ServantError a)) -> String -> [TrailEntry] -> IO ()
checkTrail step http differentator trail = do
  step $ "That adding " <> differentator <> " trail works."
  addEntryResult <- http $ addTrail trail
  addEntryResult `shouldBe` Right NoContent

  step $ "That getting a " <> differentator <> " trail by signature works."
  getEntryBySignatureResult <- http $ getTrailBySignature (trailEntrySignature $ head trail)
  getEntryBySignatureResult `shouldMatchTrail` trail

  step $ "That getting a " <> differentator <> " trail by eventId works."
  getEntryByEventIdResult <- http $ getTrailByEventId (trailEntryEventID $ head trail)
  getEntryByEventIdResult `shouldMatchTrail` trail
