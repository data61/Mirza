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
          getSingleEntryByEventIdResult <- http $ getTrailByEventId (trailEntryEventId singleEntry)
          getSingleEntryByEventIdResult `shouldMatchTrail` [singleEntry]


          let checkTrailWithContext = checkTrail step http
          let checkDistinctTrailsCommonEventIdWithContext = checkDistinctTrailsCommonEventId step http

          -- Trail: *---*
          let buildTwoEntryTrail = addNextEntryIO $ buildSingleEntryTrail
          twoEntryTrail <- buildTwoEntryTrail
          checkTrailWithContext "2 Entry Trail (1 Previous Entry)" twoEntryTrail

          -- Trail: *---*---*
          let buildThreeEntryTrail = addNextEntryIO $ buildTwoEntryTrail
          threeEntryTrail <- buildThreeEntryTrail
          checkTrailWithContext "3 Entry Trail (1 Previous Entry, 1 Next Entry)" threeEntryTrail

          -- Trail: *--\
          --            *
          --        *--/
          let buildTwoPreviousEntryTrail = addPreviousEntryIO $ buildTwoEntryTrail
          twoPreviousEntryTrail <- buildTwoPreviousEntryTrail
          checkTrailWithContext "2 Previous Entries Trail" twoPreviousEntryTrail

          -- Trail: *--\
          --        *---*
          --        *--/
          threePreviousEntryTrail <- addPreviousEntryIO $ buildTwoPreviousEntryTrail
          checkTrailWithContext "3 Previous Entries Trail" threePreviousEntryTrail

          -- Trail:  /--*
          --        *
          --         \--*
          let buildTwoNextEntryTrail = addNextEntryIO $ swapIO $ buildTwoEntryTrail
          twoNextEntryTrail <- buildTwoNextEntryTrail
          checkTrailWithContext "2 Next Entries Trail" twoNextEntryTrail

          -- Trail:  /--*
          --        *---*
          --         \--*
          threeNextEntryTrail <- addNextEntryIO $ swapIO $ buildTwoNextEntryTrail
          checkTrailWithContext "3 Next Entries Trail" threeNextEntryTrail

          -- Trail: *--\ /--*
          --            *
          --        *--/ \--*
          twoPreviousTwoNextEntryTrail <- addNextEntryIO $ swapIO $ addNextEntryIO $ buildTwoPreviousEntryTrail
          checkTrailWithContext "2 Previous 2 Next Entries Trail" twoPreviousTwoNextEntryTrail

          -- Trail: *--\     /--*
          --            *---*
          --        *--/     \--*
          twoPreviousThenNextThenTwoNextEntryTrail <- addNextEntryIO $ swapIO $ addNextEntryIO $ addNextEntryIO $ buildTwoPreviousEntryTrail
          checkTrailWithContext "1 Previous Entry, then 2 Previous Entries and 2 Next Entries Trail" twoPreviousThenNextThenTwoNextEntryTrail

          -- Trail: *---*--\     /--*---*
          --                *---*
          --        *---*--/     \--*---*
          let buildLongWing = do
                           topInput <- buildThreeEntryTrail
                           bottomInput <- buildTwoEntryTrail
                           let inputWing = joinEntries (firstSignature bottomInput) topInput <> bottomInput
                           inputArrow <- addNextEntry inputWing
                           inputAndTopOutput <- addNextEntryIO $ addNextEntry inputArrow
                           bottomOutput <- addNextEntryIO $ joinEntriesIO (firstSignature inputArrow) $ buildSingleEntryTrail
                           pure $ inputAndTopOutput <> bottomOutput
          longWing <- buildLongWing
          checkTrailWithContext "Long Wing Trail (see code comment diagram)" longWing

          -- Trail:  /--*--\
          --        *       *
          --         \--*--/
          let buildRing = do
                            forkedNext <- buildTwoNextEntryTrail
                            newEntry <- buildEntry
                            let joinEnd joinedEntry entry = case (trailEntryParentSignatures entry) of
                                                         [] -> joinEntry (trailEntrySignature entry) joinedEntry
                                                         _  -> joinedEntry
                            let joinedEntry = foldl joinEnd newEntry forkedNext
                            pure $ joinedEntry : forkedNext
          ringTrail <- buildRing
          checkTrailWithContext "Ring Trail (see code comment diagram)" ringTrail

          -- Trail:  /--*--\
          --        *-------*
          --         \--*--/
          let buildBurger = do
                              ring <- buildRing
                              let (Just base) = find (([] ==) . trailEntryParentSignatures) ring
                              let makeBaseParentOf entry = if length (trailEntryParentSignatures entry) == 2 then
                                                             joinEntry (trailEntrySignature base) entry
                                                           else
                                                             entry
                              pure $ (makeBaseParentOf <$> ring)
          burgerTrail <- buildBurger
          checkTrailWithContext "Burger Trail (see code comment diagram)" burgerTrail

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
          let buildLattice = do
                                topInput <- buildTwoEntryTrail
                                bottomInput <- buildTwoEntryTrail
                                topOutputNode <- buildEntry
                                topOutput <- addNextEntry $ pure $ joinEntry (firstSignature topInput) topOutputNode
                                outputNode <- buildEntry
                                bottomOutput <- addNextEntry $ pure $ foldl (flip joinEntry) outputNode (firstSignature <$> [topInput, bottomInput])
                                pure $ topOutput <> bottomOutput <> topInput <> bottomInput
          latticeTrail <- buildLattice
          checkTrailWithContext "Lattice Trail (see code comment diagram)" latticeTrail

          -- Trail: *---*---*
          --            :
          --        *---*---*
          -- Note: ':' Denotes matching eventId (but otherwise distinct trails).
          commonEventIdDistinctTrailsTopInput <- buildTwoEntryTrail
          let commonEventIdDistinctTrailsMatchingTrailId = firstEventId commonEventIdDistinctTrailsTopInput
          commonEventIdDistinctTrailsBottomTrail <- addNextEntryIO $ updateFirstEventIdIO commonEventIdDistinctTrailsMatchingTrailId $ buildTwoEntryTrail
          commonEventIdDistinctTrailsTopTrail <- addNextEntry $ commonEventIdDistinctTrailsTopInput
          checkDistinctTrailsCommonEventIdWithContext "mid" commonEventIdDistinctTrailsTopTrail commonEventIdDistinctTrailsBottomTrail commonEventIdDistinctTrailsMatchingTrailId

          -- Trail: *---*---*
          --        :
          --        *---*---*
          -- Note: ':' Denotes matching eventId (but otherwise distinct trails).
          commonEventIdStartDistinctTrailsTopInput <- buildSingleEntryTrail
          let commonEventIdStartDistinctTrailsMatchingTrailId  = firstEventId commonEventIdStartDistinctTrailsTopInput
          commonEventIdStartDistinctTrailsBottomInput <- updateFirstEventIdIO commonEventIdStartDistinctTrailsMatchingTrailId $ buildSingleEntryTrail
          commonEventIdStartDistinctTrailsTopTrail <- addNextEntryIO $ addNextEntry commonEventIdStartDistinctTrailsTopInput
          commonEventIdStartDistinctTrailsBottomTrail <- addNextEntryIO $ addNextEntry commonEventIdStartDistinctTrailsBottomInput
          -- traceM $ "Top Trail: " <> (prettyTrail commonEventIdStartDistinctTrailsTopTrail)
          -- traceM $ "Bottom Trail: " <> (prettyTrail commonEventIdStartDistinctTrailsBottomTrail)
          -- traceM $ "CommonEventId: " <> (show commonEventIdStartDistinctTrailsMatchingTrailId)
          checkDistinctTrailsCommonEventIdWithContext "at the start of the" commonEventIdStartDistinctTrailsTopTrail commonEventIdStartDistinctTrailsBottomTrail commonEventIdStartDistinctTrailsMatchingTrailId

          -- Trail: *---*---*
          --                :
          --        *---*---*
          -- Note: ':' Denotes matching eventId (but otherwise distinct trails).
          commonEventIdEndDistinctTrailsTopTrail <- buildThreeEntryTrail
          commonEventIdEndDistinctTrailsBottomInput <- buildThreeEntryTrail
          let commonEventIdEndDistinctTrailsMatchingTrailId  = firstEventId commonEventIdEndDistinctTrailsTopTrail
          let commonEventIdEndDistinctTrailsBottomTrail = updateFirstEventId commonEventIdEndDistinctTrailsMatchingTrailId commonEventIdEndDistinctTrailsBottomInput
          checkDistinctTrailsCommonEventIdWithContext "at the end of the" commonEventIdEndDistinctTrailsTopTrail commonEventIdEndDistinctTrailsBottomTrail commonEventIdEndDistinctTrailsMatchingTrailId

          -- Trail: *---*---\
          --            :    *
          --        *---*---/
          -- Note: ':' Denotes matching eventId (but otherwise distinct trail entries).
          commonEventIdJoinedEndTopInput <- buildTwoEntryTrail
          let commonEventIdJoinedEndMatchingTrailId = firstEventId commonEventIdJoinedEndTopInput
          commonEventIdJoinedEndBottomTrail <- addNextEntryIO $ updateFirstEventIdIO commonEventIdJoinedEndMatchingTrailId $ buildTwoEntryTrail
          let commonEventIdJoinedEndTopTrail = joinEntries (firstSignature commonEventIdJoinedEndBottomTrail) commonEventIdJoinedEndTopInput
          let completeCommentEventIdJoinedEndTrail = commonEventIdJoinedEndTopTrail <> commonEventIdJoinedEndBottomTrail
          checkTrailWithContext "Common EventId Joined End Trail" completeCommentEventIdJoinedEndTrail

          -- Trail:  /--*---*
          --        *   :
          --         \--*---*
          -- Note: ':' Denotes matching eventId (but otherwise distinct trail entries).
          let buildCommonEventIdJoinedStart = do
                                  root       <- buildEntry
                                  topNext    <- joinEntriesIO (trailEntrySignature root) $ buildSingleEntryTrail
                                  bottomNext <- updateFirstEventIdIO (firstEventId topNext) $ (joinEntriesIO (trailEntrySignature root) $ buildSingleEntryTrail)
                                  topBoth    <- addNextEntry $ topNext
                                  bottomBoth <- addNextEntry $ bottomNext
                                  pure $ bottomBoth <> topBoth <> [root]
          commonEventIdJoinedStart <- buildCommonEventIdJoinedStart
          --traceM $ prettyTrail commonEventIdJoinedStart
          checkTrailWithContext "Common EventId Joined Start Trail" commonEventIdJoinedStart

          -- Trail:  /--*--\
          --        *   :   *
          --         \--*--/
          -- Note: ':' Denotes matching eventId (but otherwise distinct trail entries).
          let buildCommonEventIdJoinedStartEnd = do
                                  root       <- buildEntry
                                  topNext    <- joinEntriesIO (trailEntrySignature root) $ buildSingleEntryTrail
                                  bottomNext <- updateFirstEventIdIO (firstEventId topNext) $ (joinEntriesIO (trailEntrySignature root) $ buildSingleEntryTrail)
                                  topEnd    <- joinEntriesIO (firstSignature bottomNext) $ (addNextEntry $ topNext)
                                  pure $ topEnd <> bottomNext <> [root]
          commonEventIdJoinedStartEnd <- buildCommonEventIdJoinedStartEnd
          --traceM $ prettyTrail commonEventIdJoinedStartEnd
          checkTrailWithContext "Common EventId Joined Start and End of Trail" commonEventIdJoinedStartEnd



  -- Test that invalid signature fails.
  -- Test that invalid version fails.
  -- Test that timestamp in the future is invalid.
  -- Test when trying to add an entry where the parent isn't stored in the service fails.
  -- That if the entry version is not 1 adding fails.
  -- Test that if the same id appears in the parentID multiple times thet the service fails.
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



--------------------------------------------------------------------------------
-- Some utility functions to make construction of the test cases simpler
--------------------------------------------------------------------------------

-- Note: These utility functions are only simple and operate on the operands specified only, they do not propogate
--       signature updates along the tree and as so when constructing test trails using them they must always be
--       constructed from origin to destination.

buildSingleEntryTrail :: IO [TrailEntry]
buildSingleEntryTrail = pure <$> buildEntry


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
buildSignature entry = SignaturePlaceholder $ "SignaturePlaceholder-" <> (toText $ unEventId $ trailEntryEventId entry)


-- This function is just designed to simplify expression, see addNextEntry comment.
addNextEntryIO :: IO [TrailEntry] -> IO [TrailEntry]
addNextEntryIO trail = join $ addNextEntry <$> trail


-- Adds the new next entry to the element at the start of the list. The new entry is placed at the start of the
-- list and the remaining entries will be appended after the new element (retaining their original order).
addNextEntry :: [TrailEntry] -> IO [TrailEntry]
addNextEntry entries@(entry : _) = do
  newEntry <- buildEntry
  let updatedNewEntryWithPreviousEntry = joinEntry (trailEntrySignature entry) newEntry
  pure $ updatedNewEntryWithPreviousEntry : entries
-- Could just define the following as buildEntry, but it seems that this is likely to be a logic error and so its probably better to just fail here.
addNextEntry [] = error "Error: There is a logic error in the tests. Can't add the next entry of a non existant entry."


-- This function is just designed to simplify expression, see addPreviousEntry comment.
addPreviousEntryIO :: IO [TrailEntry] -> IO [TrailEntry]
addPreviousEntryIO trail = join $ addNextEntry <$> trail


-- Adds a new entry which is the previous entry for the first entry in the trail. The first element in the trail will
-- retain its position and then the new element (previous of the first entry) will be added after this and then all of
-- the remaining entries will be appeneded after this in there original order.
addPreviousEntry :: [TrailEntry] -> IO [TrailEntry]
addPreviousEntry entries = do
  newEntry <- buildEntry
  pure $ swap $ newEntry : (joinEntries (trailEntrySignature newEntry) entries)


-- This function is just designed to simplify expression, see joinEntries comment.
joinEntriesIO :: SignaturePlaceholder -> IO [TrailEntry] -> IO [TrailEntry]
joinEntriesIO sig = fmap (joinEntries sig)


-- Adds the supplied signature to the first entries list of parents
joinEntries :: SignaturePlaceholder -> [TrailEntry] -> [TrailEntry]
joinEntries sig (entry : entries) = (joinEntry sig entry) : entries
-- Could just define the following as buildEntry, but it seems that this is likely to be a logic error and so its probably better to just fail here.
joinEntries _ [] = error "Error: There is a logic error in the tests. Can't add a previous entry of a non existant entry."


-- Adds the supplied signature to the entry's list of parents
joinEntry :: SignaturePlaceholder -> TrailEntry -> TrailEntry
joinEntry sig entry = entry{trailEntryParentSignatures = sig : (trailEntryParentSignatures entry)}


-- This function is just designed to simplify expression, see updateFirstEventId comment.
updateFirstEventIdIO :: EventId -> IO [TrailEntry] -> IO [TrailEntry]
updateFirstEventIdIO eventId entries = updateFirstEventId eventId <$> entries


-- Updates the eventId of the first entry in the trail. The order of the trail is retained.
updateFirstEventId :: EventId -> [TrailEntry] -> [TrailEntry]
updateFirstEventId eventId (entry : entries) = (updateEventId eventId entry) : entries
-- Could just define the following as NOP, but it seems that this is likely to be a logic error and so its probably better to just fail here.
updateFirstEventId _ [] = error "Error: There is a logic error in the tests. Can't add the update the EventId a non existant entry."


-- Updates the eventId of the entry.
updateEventId :: EventId -> TrailEntry -> TrailEntry
updateEventId eventId entry = entry{trailEntryEventId = eventId} -- TODO: Need to resign at this point...


-- This function is just designed to simplify expression.
-- Gets the signature of the first entry in the trail list.
firstSignature :: [TrailEntry] -> SignaturePlaceholder
firstSignature = firstField trailEntrySignature


-- This function is just designed to simplify expression.
-- Gets the eventId of the first entry in the trail list.
firstEventId :: [TrailEntry] -> EventId
firstEventId = firstField trailEntryEventId


-- This function is just designed to simplify expression.
firstField :: (TrailEntry -> a) -> [TrailEntry] -> a
firstField _ [] = error "Error: There is a logic error in the tests. Can't get the first field in an empty trail."
firstField fn entries = fn $ head entries


-- This function is just designed to simplify expression, see swap comment.
swapIO :: IO [TrailEntry] -> IO [TrailEntry]
swapIO = fmap swap


-- Swaps the order of the first two elements in the trail.
swap :: [TrailEntry] -> [TrailEntry]
swap []                      = []
swap list@[_]                = list
swap (first : second : rest) = second : first : rest



--------------------------------------------------------------------------------
-- Some utility functions to make expression of the test cases simpler
--------------------------------------------------------------------------------

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
checkTrail step http differentator trail = checkPartialTrail step http differentator trail trail (trailEntrySignature <$> trail) (trailEntryEventId <$> trail)


checkPartialTrail :: (String -> IO()) -> (forall a. ClientM a -> IO (Either ServantError a)) -> String -> [TrailEntry] -> [TrailEntry] -> [SignaturePlaceholder] -> [EventId] -> IO ()
checkPartialTrail step http differentator inputTrail expectedTrail sigs eventIds = do
  step $ "That adding " <> differentator <> " trail works."
  addEntryResult <- http $ addTrail inputTrail
  addEntryResult `shouldBe` Right NoContent

  step $ "That getting a " <> differentator <> " trail by (each of the) signature(s) works."
  getEntryBySignatureResult <- traverse (\sig -> http $ getTrailBySignature sig) sigs
  signatureResults <- traverse (`shouldMatchTrail` expectedTrail) getEntryBySignatureResult
  pure $ forceElements signatureResults

  step $ "That getting a " <> differentator <> " trail by (each of the) eventId(s) works."
  getEntryByEventIdResult <- traverse (\eventId -> http $ getTrailByEventId eventId) eventIds
  eventIdResults <- traverse (`shouldMatchTrail` expectedTrail) getEntryByEventIdResult
  pure $ forceElements eventIdResults


checkDistinctTrailsCommonEventId :: (String -> IO()) -> (forall a. ClientM a -> IO (Either ServantError a)) -> String -> [TrailEntry] -> [TrailEntry] -> EventId -> IO ()
checkDistinctTrailsCommonEventId step http differentator topTrail bottomTrail commonEventId = do
  let filterNotMatchingTrailId matchingTrailId trail = filter (/= matchingTrailId) $ trailEntryEventId <$> trail

  let completeTrail = topTrail <> bottomTrail
  -- traceM $ "\nCompleteTrail: " <> (prettyTrail completeTrail)
  let topTrailUniqueEventIds    = filterNotMatchingTrailId commonEventId topTrail
  let bottomTrailUniqueEventIds = filterNotMatchingTrailId commonEventId bottomTrail

  let checkPartialTrailWithContext = checkPartialTrail step http
  checkPartialTrailWithContext ("Two distinct trails with a common EventId " <> differentator <> " trail (top trail)")      completeTrail topTrail      (trailEntrySignature <$> topTrail   ) topTrailUniqueEventIds
  checkPartialTrailWithContext ("Two distinct trails with a common EventId " <> differentator <> " trail (bottom trail)")   completeTrail bottomTrail   (trailEntrySignature <$> bottomTrail) bottomTrailUniqueEventIds
  checkPartialTrailWithContext ("Two distinct trails with a common EventId " <> differentator <> " trail (common EventId)") completeTrail completeTrail [] [commonEventId]



--------------------------------------------------------------------------------
-- Some utility functions to display the trails nicely for debugging
--------------------------------------------------------------------------------

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
