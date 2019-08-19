{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


module Mirza.Trails.Handlers.Trails where


import           Mirza.Trails.Types
import           Mirza.Trails.Database.Schema

import           Mirza.Common.Types
import           Mirza.Common.Time
import           Mirza.Common.Utils

import           Data.GS1.EventId                         (EventId (..))

import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Query                      hiding (time)

import           Servant

import           Control.Lens.Extras
import           Control.Lens.Type (AReview)
import           Control.Monad.Error.Lens                 (throwing_)

import           Control.Monad.Identity

import           Data.Maybe
import           Data.List                                (nub)


getTrailByEventId :: ( Member context '[HasEnvType, HasConnPool, HasKatipContext, HasKatipLogEnv]
                     , Member err '[AsTrailsServiceError, AsSqlError])
                  => EventId ->  AppM context err [TrailEntry]
getTrailByEventId eventId = do
  runDb $ (getTrailByEventIdQuery eventId)


getTrailByEventIdQuery :: (AsTrailsServiceError err)
                         => EventId -> DB context err [TrailEntry]
getTrailByEventIdQuery eventId = do
  entryList <- pg $ runSelectReturningList $ select $ do
            entry <- all_ (_entries trailsDB)
            guard_ (entries_event_id entry ==. val_ (unEventId eventId))
            pure entry
  case entryList of
    [] -> throwing_ _EventIdNotFoundTSE
    entry -> build getTrailBySignatureQuery [] (entries_signature <$> entry)


getTrailBySignature :: ( Member context '[HasEnvType, HasConnPool, HasKatipContext, HasKatipLogEnv]
                       , Member err '[AsTrailsServiceError, AsSqlError])
                    => SignaturePlaceholder ->  AppM context err [TrailEntry]
getTrailBySignature sig = do
  runDb $ (getTrailBySignatureQuery [] sig)


-- Algorithm is basically get a node (TrailEntry), get all of the previous and following entries, if we haven't seen
-- them before then recurse (if we have seen them before ignore them). We need to do this in a statefull way so that we
-- can handle loop cases (cases where the trail meets in a loop). I realised after I had a working implementation that a
-- hash map (rather then list) was probably a better data structure to use here, but as the implementation is done and
-- works and the expected lifetime of this code it seems wastefull to spend any more time on the implementation at present.
getTrailBySignatureQuery :: (AsTrailsServiceError err)
                         => [TrailEntry] -> SignaturePlaceholder -> DB context err [TrailEntry]
getTrailBySignatureQuery discovered searchSignature = do
  previousEntries <- getThisAndPreviousEntriesBySignatureQuery discovered searchSignature
  getNextEntriesBySignatureQuery previousEntries searchSignature


getNextEntriesBySignatureQuery :: (AsTrailsServiceError err)
                => [TrailEntry] -> SignaturePlaceholder -> DB context err [TrailEntry]
getNextEntriesBySignatureQuery discovered searchSignature = do
  followingSignatures <- pg $ runSelectReturningList $ select $ do
                          previous <- all_ (_previous trailsDB)
                          guard_ (previous_previous_signature previous ==. val_ searchSignature)
                          pure $ (previous_entry_signature previous)

  let newFollowingSignatures = filter (isNotPresentIn discovered) (entriesPrimaryKeyToSignature <$> followingSignatures)
  build getTrailBySignatureQuery discovered newFollowingSignatures


getThisAndPreviousEntriesBySignatureQuery :: (AsTrailsServiceError err)
                           => [TrailEntry] -> SignaturePlaceholder -> DB context err [TrailEntry]
getThisAndPreviousEntriesBySignatureQuery discovered searchSignature = do
  if not $ elem searchSignature (trailEntrySignature <$> discovered) then do
    entry <- getEntryBySignature searchSignature
    let combined = discovered <> [entry]
    let previous = filter (isNotPresentIn combined) $ trailEntryPreviousSignatures entry
    build getTrailBySignatureQuery combined previous
  else
    pure discovered


getEntriesBySignature :: (AsTrailsServiceError err)
                      => SignaturePlaceholder -> DB context err [TrailEntry]
getEntriesBySignature searchSignature = pure <$> (getEntryBySignature searchSignature)

getEntryBySignature :: (AsTrailsServiceError err)
                    => SignaturePlaceholder -> DB context err TrailEntry
getEntryBySignature searchSignature = do
  maybeEntry <- pg $ runSelectReturningOne $ select $ do
            entry <- all_ (_entries trailsDB)
            guard_ (entries_signature entry ==. val_ searchSignature)
            pure entry
  case maybeEntry of
    Nothing    -> throwing_ _SignatureNotFoundTSE
    Just entry -> do
                    previous <- pg $ runSelectReturningList $ select $ do
                                 previous <- all_ (_previous trailsDB)
                                 guard_ (previous_entry_signature previous ==. val_ (EntriesPrimaryKey searchSignature))
                                 pure previous
                    pure $ buildTrailEntry entry previous


-- I'm sure that there is a nicer "implemenentation" of this function, something like foldl >>=, but I can't find it right now so this will do and can always refactor this later.
build :: (Monad m) =>  ([a] -> b -> m [a]) -> [a] -> [b] -> m [a]
build _ discovered [] = pure discovered
build fn discovered (sig : rest) = do
  thisEntry <- fn discovered sig
  build fn thisEntry rest


isNotPresentIn :: [TrailEntry] -> SignaturePlaceholder -> Bool
isNotPresentIn discovered element = not $ elem element $ trailEntrySignature <$> discovered


buildTrailEntry :: Entries -> [Previous] -> TrailEntry
buildTrailEntry entries previous = TrailEntry 1
                                          (onLocalTime EntryTime $ entries_timestamp entries)
                                          (entries_gs1company_prefix entries)
                                          (EventId $ entries_event_id entries)
                                          (previous_previous_signature <$> previous)
                                          (entries_signature entries)


addTrail  :: ( Member context '[HasEnvType, HasConnPool, HasKatipContext, HasKatipLogEnv]
             , Member err '[AsTrailsServiceError, AsSqlError])
          => [TrailEntry] ->  AppM context err NoContent
addTrail trail = do
  _ <- runDb $ (addEntryQuery trail)
  pure NoContent


throwing_If :: MonadError e m => Control.Lens.Type.AReview e () -> Bool -> m ()
throwing_If x result = if result then
                         throwing_ x
                       else
                         pure ()


addEntryQuery :: (AsTrailsServiceError err)
              => [TrailEntry] -> DB context err ()
addEntryQuery entriesRaw = do
  let ignoreNotFound = handleError (\err -> if is _SignatureNotFoundTSE err then pure Nothing else throwError err)
  existingEntries <- fmap catMaybes $ traverse (ignoreNotFound . (fmap Just <$> getEntryBySignature)) (trailEntrySignature <$> entriesRaw)
  -- Note: We only need to check if the signature exists (and not that the full event contents match) since the signature guarantees that they events are the same.
  let newEntries = filter (\entry -> not $ elem (trailEntrySignature entry) $ trailEntrySignature <$> existingEntries) entriesRaw

  -- Check that the event versions are all valid.
  throwing_If _InvalidEntryVersionTSE $ not $ all (== 1) $ trailEntryVersion <$> newEntries

  -- TODO: nub is documented to have order O(n^2) and should probably be replaced before this service is productionised (there are better algorithms for this use case).
  let uniqueNewEntries = nub newEntries

  -- TODO: Again nub is documented to have order O(n^2) and should probably be replaced before this service is productionised (there are better algorithms for this use case).
  let previousSignatures = (trailEntryPreviousSignatures <$> uniqueNewEntries)
  throwing_If _DuplicatePreviousEntriesTSE ((nub <$> previousSignatures) /= previousSignatures)

  -- This constraint could possibly be handled more efficently by letting the DB check the constraints directly and then
  -- transform the error returned.
  previousExist <- validPrevious newEntries
  throwing_If _PreviousEntryNotFoundTSE $ (not previousExist)

  -- Add the entries to the DB.
  let entries = trailEntryToEntriesT <$> uniqueNewEntries
  let previous = concat $ trailEntryToPreviousT <$> uniqueNewEntries
  _ <- pg $ runInsertReturningList $ insert (_entries trailsDB)
          $ insertValues entries
  _ <- pg $ runInsertReturningList $ insert (_previous trailsDB)
          $ insertValues previous
  pure ()


validPrevious :: [TrailEntry] -> DB context err Bool
validPrevious entries = do
    let searchPrevious sig = pg $ runSelectReturningOne $ select $ do
                             entry <- all_ (_entries trailsDB)
                             guard_ (entries_signature entry ==. val_ sig)
                             pure entry
    previous <- traverse searchPrevious (unmatchedPrevious entries)
    pure $ (all isJust previous)


unmatchedPrevious :: [TrailEntry] -> [SignaturePlaceholder]
unmatchedPrevious entries = catMaybes $ (\previous -> if elem previous signatures then Nothing else Just previous) <$> requiredPrevious where
                           signatures = trailEntrySignature <$> entries
                           requiredPrevious = concat $ trailEntryPreviousSignatures <$> entries


trailEntryToEntriesT :: TrailEntry -> EntriesT Identity
trailEntryToEntriesT trailEntry = EntriesT (trailEntrySignature trailEntry)
                                           (toDbTimestamp $ getEntryTime $ trailEntryTimestamp trailEntry)
                                           (trailEntryGS1CompanyPrefix trailEntry)
                                           (unEventId $ trailEntryEventId trailEntry)
                                           Nothing


trailEntryToPreviousT :: TrailEntry -> [PreviousT Identity]
trailEntryToPreviousT trailEntry = (PreviousT (EntriesPrimaryKey $ trailEntrySignature trailEntry)) <$> (trailEntryPreviousSignatures trailEntry)
