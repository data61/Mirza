{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Mirza.Trails.Tests.Client where


import           Mirza.Trails.Tests.InitClient

import           Mirza.Trails.Client.Servant
import           Mirza.Trails.Types

import           Mirza.Common.Tests.ServantUtils

import           Mirza.Common.Tests.Utils        (checkFailureMessage,
                                                  checkFailureStatus,
                                                  secondsToMicroseconds,
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

import           Crypto.Hash

import           System.Random

import           Control.Concurrent              (threadDelay)
import           Control.Exception               (bracket)
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8           (pack)
import           Data.Either                     (isLeft, isRight)
import           Data.Foldable
import           Data.List
import           Data.Text                       (pack)
import           Data.Time.Clock
import           Data.UUID.V4

-- import           Debug.Trace


-- === Trails Servant Client tests
clientSpec :: IO TestTree
clientSpec = do
  let trailTests = testCaseSteps "Trails Endpoints" $ \step ->
        bracket runTrailsApp (\(a,b) -> endWaiApp (a,b)) $ \(_tid, baseurl) -> do
          let http = runClient baseurl

          -- These test cases should largely be property based tests, but as we currently have no property based testing
          -- machinery and time constraints of the project we just implement a bunch of copy pasted simple tests to
          -- check a variety of classic test boundry scenarios.

          step "That when there are no entries in the database that getting by signature responds corretly"
          getGetSignatureInitialEmpty <- http $ getTrailBySignature (SignaturePlaceholder "invalid")
          getGetSignatureInitialEmpty `shouldSatisfy` isLeft
          getGetSignatureInitialEmpty `shouldSatisfy` (checkFailureStatus NS.notFound404)
          getGetSignatureInitialEmpty `shouldSatisfy` (checkFailureMessage "A trail with a matching signature was not found.")

          step "That when there are no entries in the database that getting by eventId responds corretly"
          empty_non_matching_uuid <- liftIO nextRandom
          getGetEventIdInitialEmpty <- http $ getTrailByEventId (EventId empty_non_matching_uuid)
          getGetEventIdInitialEmpty `shouldSatisfy` isLeft
          getGetEventIdInitialEmpty `shouldSatisfy` (checkFailureStatus NS.notFound404)
          getGetEventIdInitialEmpty `shouldSatisfy` (checkFailureMessage "A trail with the matching EventId was not found.")


          -- Trail: *
          step "That adding the first entry in a trail works"
          singleEntry <- buildEntry
          verifyValidTrailTestIntegrityCheck [singleEntry]
          addFirstEntryResult <- http $ addTrail [singleEntry]
          addFirstEntryResult `shouldBe` Right NoContent

          step "That getting a single entry trail by signature works"
          getSingleEntryBySignatureResult <- http $ getTrailBySignature (trailEntrySignature singleEntry)
          getSingleEntryBySignatureResult `shouldMatchTrail` [singleEntry]

          step "That getting a single entry trail by eventId works"
          getSingleEntryByEventIdResult <- http $ getTrailByEventId (trailEntryEventId singleEntry)
          getSingleEntryByEventIdResult `shouldMatchTrail` [singleEntry]


          let checkTrailWithContext = checkTrail step http
          let checkDistinctTrailsCommonEventIdWithContext = checkDistinctTrailsCommonEventId step http

          -- Trail: *---*
          let buildTwoEntryTrail = addNextEntryIO $ buildSingleEntryTrail
          twoEntryTrail <- buildTwoEntryTrail
          --traceM $ "Debug: " <> (prettyTrail twoEntryTrail)
          checkTrailWithContext "2 Entry Trail (1 Previous Entry)" twoEntryTrail

          -- Trail: *---*---*
          let buildThreeEntryTrail = addNextEntryIO $ buildTwoEntryTrail
          threeEntryTrail <- buildThreeEntryTrail
          --traceM $ "Debug: " <> (prettyTrail threeEntryTrail)
          checkTrailWithContext "3 Entry Trail (1 Previous Entry, 1 Next Entry)" threeEntryTrail

          -- Trail: *--\
          --            *
          --        *--/
          let buildTwoPreviousEntryTrail = addPreviousEntryIO $ buildTwoEntryTrail
          twoPreviousEntryTrail <- buildTwoPreviousEntryTrail
          -- traceM $ "Debug: " <> (prettyTrail twoPreviousEntryTrail)
          checkTrailWithContext "2 Previous Entries Trail" twoPreviousEntryTrail

          -- Trail: *--\
          --        *---*
          --        *--/
          threePreviousEntryTrail <- addPreviousEntryIO $ buildTwoPreviousEntryTrail
          -- traceM $ "Debug: " <> (prettyTrail threePreviousEntryTrail)
          checkTrailWithContext "3 Previous Entries Trail" threePreviousEntryTrail

          -- Trail:  /--*
          --        *
          --         \--*
          let buildTwoNextEntryTrail = addNextEntryIO $ swapIO $ buildTwoEntryTrail
          twoNextEntryTrail <- buildTwoNextEntryTrail
          -- traceM $ "Debug: " <> (prettyTrail twoNextEntryTrail)
          checkTrailWithContext "2 Next Entries Trail" twoNextEntryTrail

          -- Trail:  /--*
          --        *---*
          --         \--*
          threeNextEntryTrail <- addNextEntryIO $ swapIO $ buildTwoNextEntryTrail
          -- traceM $ "Debug: " <> (prettyTrail threeNextEntryTrail)
          checkTrailWithContext "3 Next Entries Trail" threeNextEntryTrail

          -- Trail: *--\ /--*
          --            *
          --        *--/ \--*
          twoPreviousTwoNextEntryTrail <- addNextEntryIO $ swapIO $ addNextEntryIO $ buildTwoPreviousEntryTrail
          -- traceM $ "Debug: " <> (prettyTrail twoPreviousTwoNextEntryTrail)
          checkTrailWithContext "2 Previous 2 Next Entries Trail" twoPreviousTwoNextEntryTrail

          -- Trail: *--\     /--*
          --            *---*
          --        *--/     \--*
          twoPreviousThenNextThenTwoNextEntryTrail <- addNextEntryIO $ swapIO $ addNextEntryIO $ addNextEntryIO $ buildTwoPreviousEntryTrail
          -- traceM $ "Debug: " <> (prettyTrail twoPreviousThenNextThenTwoNextEntryTrail)
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
          -- traceM $ "Debug: " <> (prettyTrail longWing)
          checkTrailWithContext "Long Wing Trail (see code comment diagram)" longWing

          -- Trail:  /--*--\
          --        *       *
          --         \--*--/
          let buildRing = do
                            forkedNext <- buildTwoNextEntryTrail
                            newEntry <- buildEntry
                            let joinEnd joinedEntry entry = case (trailEntryPreviousSignatures entry) of
                                                         [] -> joinedEntry
                                                         _  -> joinEntry (trailEntrySignature entry) joinedEntry
                            let joinedEntry = foldl joinEnd newEntry forkedNext
                            pure $ joinedEntry : forkedNext
          ringTrail <- buildRing
          -- traceM $ "Debug: " <> (prettyTrail ringTrail)
          checkTrailWithContext "Ring Trail (see code comment diagram)" ringTrail

          -- Trail:  /--*--\
          --        *-------*
          --         \--*--/
          let buildBurger = do
                              ring <- buildRing
                              let (Just base) = find (([] ==) . trailEntryPreviousSignatures) ring
                              let makeBasePreviousOf entry = if length (trailEntryPreviousSignatures entry) == 2 then
                                                             joinEntry (trailEntrySignature base) entry
                                                           else
                                                             entry
                              pure $ (makeBasePreviousOf <$> ring)
          burgerTrail <- buildBurger
          -- traceM $ "Debug: " <> (prettyTrail burgerTrail)
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
          -- traceM $ "Debug: " <> (prettyTrail latticeTrail)
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
          let commonEventIdJoinedEndBottomTrailJoined = joinEntries (firstSignature commonEventIdJoinedEndTopInput) commonEventIdJoinedEndBottomTrail
          let completeCommentEventIdJoinedEndTrail = commonEventIdJoinedEndTopInput <> commonEventIdJoinedEndBottomTrailJoined
          -- traceM $ "Debug: " <> (prettyTrail completeCommentEventIdJoinedEndTrail)
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
          -- traceM $ prettyTrail commonEventIdJoinedStart
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
          -- traceM $ prettyTrail commonEventIdJoinedStartEnd
          checkTrailWithContext "Common EventId Joined Start and End of Trail" commonEventIdJoinedStartEnd


          step "That adding an entry with a non 1 version fails"
          let setVersionZero entry = resign $ entry{trailEntryVersion = 0}
          zeroVersionEntry <- setVersionZero <$> buildEntry
          verifyValidTrailTestIntegrityCheck [zeroVersionEntry]
          zeroVersionEntryResult <- http $ addTrail [zeroVersionEntry]
          zeroVersionEntryResult `shouldSatisfy` isLeft
          zeroVersionEntryResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          zeroVersionEntryResult `shouldSatisfy` (checkFailureMessage "Only version 1 trail entries are currently supported by this service.")
          zeroVersionEntryGetResult <- http $ getTrailByEventId (trailEntryEventId zeroVersionEntry)
          zeroVersionEntryGetResult `shouldSatisfy` isLeft

          step "That adding an entry with a future timestamp fails"
          let futureDelay = 1
          futureTime <- (addUTCTime (fromInteger futureDelay)) <$> getCurrentTime
          let setFutureTimestamp entry = resign $ entry{trailEntryTimestamp = EntryTime futureTime}
          futureTimestampEntry <- setFutureTimestamp <$> buildEntry
          verifyValidTrailTestIntegrityCheck [futureTimestampEntry]
          futureTimestampEntryResult <- http $ addTrail [futureTimestampEntry]
          futureTimestampEntryResult `shouldSatisfy` isLeft
          futureTimestampEntryResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          futureTimestampEntryResult `shouldSatisfy` (checkFailureMessage "Only trails with timestamps that have passed may be added to this service.")
          futureTimestampEntryGetResult <- http $ getTrailByEventId (trailEntryEventId futureTimestampEntry)
          futureTimestampEntryGetResult `shouldSatisfy` isLeft
          -- Integrity Check: That after the timestamp deplay has elapsed that the entry can be entered.
          threadDelay $ fromIntegral $ secondsToMicroseconds futureDelay
          futureTimestampEntryElapsedResult <- http $ addTrail [futureTimestampEntry]
          futureTimestampEntryElapsedResult `shouldBe` Right NoContent
          futureTimestampEntryElapsedGetResult <- http $ getTrailByEventId (trailEntryEventId futureTimestampEntry)
          futureTimestampEntryElapsedGetResult `shouldMatchTrail` [futureTimestampEntry]

          step "That adding a trail with a failing entry causes the rest of the trail not to be added"
          invalidMiddleEntryTrail <- addNextEntryIO $ fmap (applyHead setVersionZero) $ addNextEntryIO $  buildSingleEntryTrail
          verifyValidTrailTestIntegrityCheck invalidMiddleEntryTrail
          invalidMiddleEntryTrailResult <- http $ addTrail invalidMiddleEntryTrail
          invalidMiddleEntryTrailResult `shouldSatisfy` isLeft
          invalidMiddleEntryTrailResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          invalidMiddleEntryTrailResult `shouldSatisfy` (checkFailureMessage "Only version 1 trail entries are currently supported by this service.")
          getEntryBySignatureInvalidMiddleEntryTrailResult <- traverse (\sig -> http $ getTrailBySignature sig) (trailEntrySignature <$> invalidMiddleEntryTrail)
          let checkNotAdded = \result -> do
                                          result `shouldSatisfy` isLeft
                                          result `shouldSatisfy` (checkFailureStatus NS.notFound404)
                                          result `shouldSatisfy` (checkFailureMessage "A trail with a matching signature was not found.")
          invalidMiddleEntryTrailCheckResult <- traverse checkNotAdded getEntryBySignatureInvalidMiddleEntryTrailResult
          pure $ forceElements invalidMiddleEntryTrailCheckResult

          step "That adding an trail with duplicate entries succeeds"
          duplicatEntry <- buildEntry
          let duplicatEntryTrail = [duplicatEntry, duplicatEntry]
          verifyValidTrailTestIntegrityCheck duplicatEntryTrail
          duplicatEntryTrailResult <- http $ addTrail duplicatEntryTrail
          duplicatEntryTrailResult `shouldBe` Right NoContent
          getDuplicateEntryBySignatureResult <- http $ getTrailBySignature (trailEntrySignature duplicatEntry)
          getDuplicateEntryBySignatureResult `shouldMatchTrail` [duplicatEntry]

          step "That adding an entry with duplicate previous entries fails"
          let dupliactePreviousSignatures entry = resign $ entry{trailEntryPreviousSignatures = (head $ trailEntryPreviousSignatures entry) : trailEntryPreviousSignatures entry}
          duplicatePreviousSignaturesBaseTrail <- addNextEntryIO $ buildSingleEntryTrail
          let duplicatePreviousSignaturesTrail = applyHead dupliactePreviousSignatures duplicatePreviousSignaturesBaseTrail
          verifyValidTrailTestIntegrityCheck duplicatePreviousSignaturesTrail
          duplicatePreviousSignaturesTrailResult <- http $ addTrail duplicatePreviousSignaturesTrail
          duplicatePreviousSignaturesTrailResult `shouldSatisfy` isLeft
          duplicatePreviousSignaturesTrailResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          duplicatePreviousSignaturesTrailResult `shouldSatisfy` (checkFailureMessage "Duplicating a signature in the previous signatures of an event is not allowed.")
          duplicatePreviousSignaturesTrailGetResult <- http $ getTrailByEventId (trailEntryEventId $ head duplicatePreviousSignaturesTrail)
          duplicatePreviousSignaturesTrailGetResult `shouldSatisfy` isLeft

          step "That adding an entry with the previous signature not in the trail, but already stored by the service succeeds"
          multiPhaseAddFirst  <- buildEntry
          multiPhaseAddSecond <- joinEntry (trailEntrySignature multiPhaseAddFirst) <$> buildEntry
          verifyValidTrailTestIntegrityCheck [multiPhaseAddFirst]
          multiPhaseAddFirstResult <- http $ addTrail [multiPhaseAddFirst]
          multiPhaseAddFirstResult `shouldBe` Right NoContent
          verifyValidTrailTestIntegrityCheck [multiPhaseAddSecond]
          multiPhaseAddSecondResult <- http $ addTrail [multiPhaseAddSecond]
          multiPhaseAddSecondResult `shouldBe` Right NoContent
          getMultiPhaseEntryBySignatureResult <- http $ getTrailBySignature (trailEntrySignature multiPhaseAddFirst)
          getMultiPhaseEntryBySignatureResult `shouldMatchTrail` [multiPhaseAddFirst, multiPhaseAddSecond]

          step "That adding an entry with the previous signatures not in the trail, and not already stored by the service fails"
          noPreviousPrevious <- buildEntry
          noPreviousEntry <- joinEntry (trailEntrySignature noPreviousPrevious) <$> buildEntry
          verifyValidTrailTestIntegrityCheck [noPreviousEntry]
          noPreviousAddResult <- http $ addTrail [noPreviousEntry]
          noPreviousAddResult `shouldSatisfy` isLeft
          noPreviousAddResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          noPreviousAddResult `shouldSatisfy` (checkFailureMessage "It is not possible to add entries with previous signatures that are not present in the current trail or already stored by the service.")
          noPreviousAddGetResult <- http $ getTrailByEventId (trailEntryEventId noPreviousEntry)
          noPreviousAddGetResult `shouldSatisfy` isLeft

          step "Test that the order of entries in a trail with parent entries in the trail submitted doesn't matter"
          -- Explicitly we want to test that the trail doesn't have to be structured so that the previous entries appear explicitly before/after they are referenced from other entries.
          trailOrder1 <- buildTwoEntryTrail
          trailOrder2 <- swapIO $ buildTwoEntryTrail
          verifyValidTrailTestIntegrityCheck trailOrder1
          verifyValidTrailTestIntegrityCheck trailOrder2
          trailOrder1Result <- http $ addTrail trailOrder1
          trailOrder1Result `shouldBe` Right NoContent
          trailOrder2Result <- http $ addTrail trailOrder2
          trailOrder2Result `shouldBe` Right NoContent
          trailOrder1GetBySignatureResult <- http $ getTrailBySignature $ firstSignature trailOrder1
          trailOrder1GetBySignatureResult `shouldMatchTrail` trailOrder1
          trailOrder2GetBySignatureResult <- http $ getTrailBySignature $ firstSignature trailOrder2
          trailOrder2GetBySignatureResult `shouldMatchTrail` trailOrder2

          -- TODO: Test that invalid signature fails.


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
  prefix <- pack <$> (sequence $ take 7 $ repeat $ randomRIO ('0', '9'))
  let unsignedEntry = TrailEntry 1
                                 (EntryTime time)
                                 (GS1CompanyPrefix prefix)
                                 (EventId uuid)
                                 []
                                 (SignaturePlaceholder "")
  pure $ resign unsignedEntry


-- This is a hack for now untril we implement signatures properly.
-- This produces really ugly JSON along the lines of:
-- Object (fromList [("signature",String ""),("org",String "12334567"),("previous_signatures",Array []),("version",Number 1.0),("event_id",String "00000000-0000-0000-0000-000000000000"),("timestamp",String "0000-00-00T00:00:00.00000000Z")])
-- the important property is that it actually uses a one way function (we use MD5 for now as a placeholder for the real signature of a nice canonical formed JSON event).
-- This function should be replaced with code that actually signs the events properly once this is determined.
buildSignature :: TrailEntry -> SignaturePlaceholder
buildSignature entry = SignaturePlaceholder $ "SignaturePlaceholder:" <> (pack $ show $ hashWith MD5 $ Data.ByteString.Char8.pack $ show $ toJSON emptySignatureEntry) where
                       emptySignatureEntry = entry{trailEntrySignature = SignaturePlaceholder ""}


resign :: TrailEntry -> TrailEntry
resign entry = entry{trailEntrySignature = buildSignature entry}


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
addPreviousEntryIO trail = join $ addPreviousEntry <$> trail


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


-- Adds the supplied signature to the first entries list of previous signatures.
joinEntries :: SignaturePlaceholder -> [TrailEntry] -> [TrailEntry]
joinEntries sig (entry : entries) = (joinEntry sig entry) : entries
-- Could just define the following as buildEntry, but it seems that this is likely to be a logic error and so its probably better to just fail here.
joinEntries _ [] = error "Error: There is a logic error in the tests. Can't add a previous entry of a non existant entry."


-- Adds the supplied signature to the entry's list of previous signatures.
joinEntry :: SignaturePlaceholder -> TrailEntry -> TrailEntry
joinEntry sig entry = resign $ entry{trailEntryPreviousSignatures = sig : (trailEntryPreviousSignatures entry)}


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
updateEventId eventId entry = resign $ entry{trailEntryEventId = eventId}


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


applyHead :: (TrailEntry -> TrailEntry) -> [TrailEntry] -> [TrailEntry]
applyHead _ [] = error "Error: There is a logic error in the tests. Can't modify the first element of an empty trail."
applyHead fn (entry : entries) = (fn entry) : entries



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
  -- Make sure that for all trails that we tests that all of the trail entries have correct valid signatures.
  verifyValidTrailTestIntegrityCheck inputTrail

  step $ "That adding " <> differentator <> " trail works"
  addEntryResult <- http $ addTrail inputTrail
  addEntryResult `shouldBe` Right NoContent

  step $ "That getting a " <> differentator <> " trail by (each of the) signature(s) works"
  getEntryBySignatureResult <- traverse (\sig -> http $ getTrailBySignature sig) sigs
  signatureResults <- traverse (`shouldMatchTrail` expectedTrail) getEntryBySignatureResult
  pure $ forceElements signatureResults

  step $ "That getting a " <> differentator <> " trail by (each of the) eventId(s) works"
  getEntryByEventIdResult <- traverse (\eventId -> http $ getTrailByEventId eventId) eventIds
  eventIdResults <- traverse (`shouldMatchTrail` expectedTrail) getEntryByEventIdResult
  pure $ forceElements eventIdResults


checkDistinctTrailsCommonEventId :: (String -> IO()) -> (forall a. ClientM a -> IO (Either ServantError a)) -> String -> [TrailEntry] -> [TrailEntry] -> EventId -> IO ()
checkDistinctTrailsCommonEventId step http differentator topTrail bottomTrail commonEventId = do
  let filterNotMatchingTrailId matchingTrailId trail = filter (/= matchingTrailId) $ trailEntryEventId <$> trail

  let completeTrail = topTrail <> bottomTrail
  -- traceM $ "CompleteTrail: " <> (prettyTrail completeTrail)
  let topTrailUniqueEventIds    = filterNotMatchingTrailId commonEventId topTrail
  let bottomTrailUniqueEventIds = filterNotMatchingTrailId commonEventId bottomTrail

  let checkPartialTrailWithContext = checkPartialTrail step http
  checkPartialTrailWithContext ("Two distinct trails with a common EventId " <> differentator <> " trail (top trail)")      completeTrail topTrail      (trailEntrySignature <$> topTrail   ) topTrailUniqueEventIds
  checkPartialTrailWithContext ("Two distinct trails with a common EventId " <> differentator <> " trail (bottom trail)")   completeTrail bottomTrail   (trailEntrySignature <$> bottomTrail) bottomTrailUniqueEventIds
  checkPartialTrailWithContext ("Two distinct trails with a common EventId " <> differentator <> " trail (common EventId)") completeTrail completeTrail [] [commonEventId]


-- This checks out test case logic to make sure that signatures are constructed correctly.
verifyValidTrailTestIntegrityCheck :: [TrailEntry] -> IO ()
verifyValidTrailTestIntegrityCheck trail = verifyValidSignaturesTrail trail `shouldBe` True


verifyValidSignaturesTrail :: [TrailEntry] -> Bool
verifyValidSignaturesTrail trail = (and $ zipWith (==) (trailEntrySignature <$> trail) (buildSignature <$> trail))



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
  <> "PreviousSignatures: " <> show previous_signatures <> "\n"
  <> "Signature: "          <> show signature           <> "\n"


prettyTrail :: [TrailEntry] -> String
prettyTrail [] = ""
prettyTrail (first : rest) = do
  prettyEntry first <> "\n"
  <> prettyTrail rest
