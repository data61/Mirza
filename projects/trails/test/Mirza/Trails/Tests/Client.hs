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
import           Test.Tasty.Runners

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
  let trailTests = testCaseSteps "TODO" $ \step ->
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
          let buildTwoEntryTrail = join $ addNextEntry <$> (fmap pure buildEntry)
          twoEntryTrail <- buildTwoEntryTrail
          checkTrailWithContext "2 Entry Trail (1 Previous Entry)" twoEntryTrail

          -- Trail: *---*---*
          let buildThreeEntryTrail = join $ addNextEntry <$> buildTwoEntryTrail
          threeEntryTrail <- buildThreeEntryTrail
          checkTrailWithContext "3 Entry Trail (1 Previous Entry, 1 Next Entry)" threeEntryTrail

          -- Trail: *--\
          --            *
          --        *--/
          let buildTwoPreviousEntryTrail = join $ addPreviousEntry <$> buildTwoEntryTrail
          twoPreviousEntryTrail <- buildTwoPreviousEntryTrail
          checkTrailWithContext "2 Previous Entries Trail" twoPreviousEntryTrail

          -- Trail: *--\
          --        *---*
          --        *--/
          threePreviousEntryTrail <- join $ addPreviousEntry <$> buildTwoPreviousEntryTrail
          checkTrailWithContext "3 Previous Entries Trail" threePreviousEntryTrail

          -- Trail:  /--*
          --        *
          --         \--*
          let buildTwoNextEntryTrail = join $ addNextEntry <$> (swap <$> buildTwoEntryTrail)
          twoNextEntryTrail <- buildTwoNextEntryTrail
          checkTrailWithContext "2 Next Entries Trail" twoNextEntryTrail

          -- Trail:  /--*
          --        *---*
          --         \--*
          threeNextEntryTrail <- join $ addNextEntry <$> (swap <$> buildTwoNextEntryTrail)
          checkTrailWithContext "3 Next Entries Trail" threeNextEntryTrail

          -- Trail: *--\ /--*
          --            *
          --        *--/ \--*
          twoPreviousTwoNextEntryTrail <- join $ fmap addNextEntry $ fmap swap $ join $ addNextEntry <$> buildTwoPreviousEntryTrail
          checkTrailWithContext "2 Previous 2 Next Entries Trail" twoPreviousTwoNextEntryTrail

          -- Trail: *--\     /--*
          --            *---*
          --        *--/     \--*
          twoPreviousThenNextThenTwoNextEntryTrail <- join $ fmap addNextEntry $ fmap swap $ join $ fmap addNextEntry $ join $ addNextEntry <$> buildTwoPreviousEntryTrail
          checkTrailWithContext "1 Previous Entry, then 2 Previous Entries and 2 Next Entries Trail" twoPreviousThenNextThenTwoNextEntryTrail

          -- Trail: *---*--\     /--*---*
          --                *---*
          --        *---*--/     \--*---*
          -- TODO:


          -- Trail:  /--*--\
          --        *       *
          --         \--*--/
          let buildRing = do
                            newEntry <- buildEntry
                            forkedPrevious <- buildTwoPreviousEntryTrail
                            let makeNewEntryParentOf entry = case trailEntryParentSignatures entry of
                                                               [] -> addPreviousEntrySignature entry (trailEntrySignature newEntry)
                                                               _  -> entry
                            pure $ [newEntry] <> (makeNewEntryParentOf <$> forkedPrevious)
          ringTrail <- buildRing
          checkTrailWithContext "Ring Trail" ringTrail


          -- Trail:  /--*--\
          --        *-------*
          --         \--*--/
          let buildBurger = do
                              ring <- buildRing
                              let (Just base) = find (([] ==) . trailEntryParentSignatures) ring
                              let makeBaseParentOf entry = if length (trailEntryParentSignatures entry) == 2 then
                                                             addPreviousEntrySignature entry (trailEntrySignature base)
                                                           else
                                                             entry
                              pure $ (makeBaseParentOf <$> ring)
          burgerTrail <- buildBurger
          checkTrailWithContext "Burger Trail" burgerTrail


          -- Trail:        *---*
          --              /
          --             /
          --        *---*
          --             \
          --              \
          --               *---*
          --              /
          --             /
          --        *---*


          -- Trail: *---*---*
          --            :
          --        *---*---*
          -- Note: ':' Denotes matching eventId (but otherwise distinct trails).
          -- TODO:


          -- Trail: *---*---\
          --            :    *
          --        *---*---/
          -- Note: ':' Denotes matching eventId (but otherwise distinct trail entries).


          -- Trail:  /--*---*
          --        *   :
          --         \--*---*
          -- Note: ':' Denotes matching eventId (but otherwise distinct trail entries).


          -- Trail:  /--*--\
          --        *   :   *
          --         \--*--/
          -- Note: ':' Denotes matching eventId (but otherwise distinct trail entries).


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
        , trailTests
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



-- Note: These utility functions are only simple and operate on the operands specified only, they do not propogate
--       signature updates along the tree and as so when constructing test trails using them they must always be
--       constructed from origin to destination.

-- This is a hack for now untril we implement signatures properly.
buildSignature :: TrailEntry -> SignaturePlaceholder
buildSignature entry = SignaturePlaceholder $ "SignaturePlaceholder-" <> (toText $ unEventId $ trailEntryEventID entry)


addPreviousEntry :: [TrailEntry] -> IO [TrailEntry]
addPreviousEntry entries = do
  newEntry <- buildEntry
  pure $ swap $ newEntry : (joinEntries entries (trailEntrySignature newEntry))


joinEntries :: [TrailEntry] -> SignaturePlaceholder -> [TrailEntry]
joinEntries (entry : entries) sig = (addPreviousEntrySignature entry sig) : entries
-- Could just define the following  as buildEntry, but it seems that this is likely to be a logic error and so its probably better to just fail here.
joinEntries [] _ = error "Error: There is a logic error in the tests. Can't add a previous entry of a non existant entry."


-- Note: Adds the new next entry to the element at the start of the list. The new element at the start of the list will remain
-- in the same position and the new element will be added after the inital element and leave the remainder of the entry
-- list in the same order following the new entry.
addNextEntry ::  [TrailEntry] -> IO [TrailEntry]
addNextEntry entries@(entry : _) = do
  newEntry <- buildEntry
  let updatedNewEntryWithPreviousEntry = addPreviousEntrySignature newEntry (trailEntrySignature entry)
  pure $ updatedNewEntryWithPreviousEntry : entries
-- Could just define the following  as buildEntry, but it seems that this is likely to be a logic error and so its probably better to just fail here.
addNextEntry [] = error "Error: There is a logic error in the tests. Can't add the next entry of a non existant entry."


addPreviousEntrySignature :: TrailEntry -> SignaturePlaceholder -> TrailEntry
addPreviousEntrySignature entry sig = entry{trailEntryParentSignatures = sig : (trailEntryParentSignatures entry)}


shouldMatchTrail :: (Show a, Eq a) => Either a [TrailEntry] -> [TrailEntry] -> Expectation
shouldMatchTrail actual@(Left _) _       = actual `shouldSatisfy` isRight
shouldMatchTrail (Right actual) expected = do
  (length actual) `shouldBe` (length expected)
  results <- sequence $ zipWith shouldMatchEntry (sort actual) (sort expected)
  pure $ forceElements results


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

  step $ "That getting a " <> differentator <> " trail by (each of the) signature(s) works."
  getEntryBySignatureResult <- traverse (\entry -> http $ getTrailBySignature (trailEntrySignature $ entry)) trail
  signatureResults <- traverse (`shouldMatchTrail` trail) getEntryBySignatureResult
  pure $ forceElements signatureResults

  step $ "That getting a " <> differentator <> " trail by (each of the) eventId(s) works."
  getEntryByEventIdResult <- traverse (\entry -> http $ getTrailByEventId (trailEntryEventID $ entry)) trail
  eventIdResults <- traverse (`shouldMatchTrail` trail) getEntryByEventIdResult
  pure $ forceElements eventIdResults


swap :: [TrailEntry] -> [TrailEntry]
swap []                      = []
swap list@[_]                = list
swap (first : second : rest) = second : first : rest


prettyEntry :: TrailEntry -> String
prettyEntry (TrailEntry  version timestamp@(EntryTime _) gs1_company_prefix eventId previous_signatures signature) =
  "Entry: "                                             <> "\n"
  <> "Version: "            <> show version             <> "\n"
  <> "Timestamp: "          <> show timestamp           <> "\n"
  <> "GS1 Company Prefix: " <> show gs1_company_prefix  <> "\n"
  <> "EventId: "            <> show eventId             <> "\n"
  <> "ParentSignatures: "   <> show previous_signatures <> "\n"
  <> "Signature: "          <> show signature           <> "\n"


prettyTrail :: [TrailEntry] -> String
prettyTrail [] = ""
prettyTrail (first : rest) = do
  prettyEntry first <> "\n"
  <> prettyTrail rest

