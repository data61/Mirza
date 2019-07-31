{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


module Mirza.Trails.Handlers.Trails where


import           Mirza.Trails.Types
import           Mirza.Trails.Database.Schema

import           Mirza.Common.Types
import           Mirza.Common.Time

import           Data.GS1.EventId                         (EventId (..))

import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam.Query                      hiding (time)

import           Servant

import           Control.Monad.Identity


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
    entry -> concat <$> traverse getTrailBySignatureQuery (entries_signature <$> entry)


getTrailBySignature :: ( Member context '[HasEnvType, HasConnPool, HasKatipContext, HasKatipLogEnv]
                       , Member err '[AsTrailsServiceError, AsSqlError])
                    => SignaturePlaceholder ->  AppM context err [TrailEntry]
getTrailBySignature sig = do
  runDb $ (getTrailBySignatureQuery sig)


getTrailBySignatureQuery :: (AsTrailsServiceError err)
                         => SignaturePlaceholder -> DB context err [TrailEntry]
getTrailBySignatureQuery searchSignature = do
  previousEntries <- getPreviousEntriesBySignatureQuery searchSignature
  followingEntries <- getPreviousEntriesQuery searchSignature
  pure $ followingEntries <> previousEntries


getPreviousEntriesQuery :: (AsTrailsServiceError err)
                => SignaturePlaceholder -> DB context err [TrailEntry]
getPreviousEntriesQuery searchSignature = do
  followingSignatures <- pg $ runSelectReturningList $ select $ do
                          previous <- all_ (_previous trailsDB)
                          guard_ (previous_previous_signature previous ==. val_ searchSignature)
                          pure $ (previous_entry_signature previous)

  entries <- traverse getEntryBySignature (entriesPrimaryKeyToSignature <$> followingSignatures)

  followingEntries <- concat <$> traverse getPreviousEntriesQuery (trailEntrySignature <$> entries)

  pure $ followingEntries <> entries


getPreviousEntriesBySignatureQuery :: (AsTrailsServiceError err)
                           => SignaturePlaceholder -> DB context err [TrailEntry]
getPreviousEntriesBySignatureQuery searchSignature = do
  entry <- getEntryBySignature searchSignature
  let previous = trailEntryParentSignatures entry
  previousEntries <- concat <$> traverse getPreviousEntriesBySignatureQuery previous
  pure $ entry : previousEntries


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


addEntryQuery :: (AsTrailsServiceError err)
              => [TrailEntry] -> DB context err ()
addEntryQuery entries_raw = do
  let entries = trailEntryToEntriesT <$> entries_raw
  let previous = concat $ trailEntryToParentsT <$> entries_raw
  _ <- pg $ runInsertReturningList $ insert (_entries trailsDB)
          $ insertValues entries
  _ <- pg $ runInsertReturningList $ insert (_previous trailsDB)
          $ insertValues previous
  pure ()


trailEntryToEntriesT :: TrailEntry -> EntriesT Identity
trailEntryToEntriesT trailEntry = EntriesT (trailEntrySignature trailEntry)
                                           (toDbTimestamp $ getEntryTime $ trailEntryTimestamp trailEntry)
                                           (trailEntryGS1CompanyPrefix trailEntry)
                                           (unEventId $ trailEntryEventID trailEntry)
                                           Nothing


trailEntryToParentsT :: TrailEntry -> [PreviousT Identity]
trailEntryToParentsT trailEntry = (PreviousT (EntriesPrimaryKey $ trailEntrySignature trailEntry)) <$> (trailEntryParentSignatures trailEntry)
