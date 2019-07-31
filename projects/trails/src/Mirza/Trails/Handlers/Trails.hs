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
                  => EventId ->  AppM context err [TrailEntryResponse]
getTrailByEventId eventId = do
  runDb $ (getTrailByEventIdQuery eventId)


getTrailByEventIdQuery :: (AsTrailsServiceError err)
                         => EventId -> DB context err [TrailEntryResponse]
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
                    => SignaturePlaceholder ->  AppM context err [TrailEntryResponse]
getTrailBySignature sig = do
  runDb $ (getTrailBySignatureQuery sig)


getTrailBySignatureQuery :: (AsTrailsServiceError err)
                         => SignaturePlaceholder -> DB context err [TrailEntryResponse]
getTrailBySignatureQuery searchSignature = do
  previousEntries <- getPreviousEntriesBySignatureQuery searchSignature
  followingEntries <- getPreviousEntriesQuery searchSignature
  pure $ followingEntries <> previousEntries


getPreviousEntriesQuery :: (AsTrailsServiceError err)
                => SignaturePlaceholder -> DB context err [TrailEntryResponse]
getPreviousEntriesQuery searchSignature = do
  followingSignatures <- pg $ runSelectReturningList $ select $ do
                          previous <- all_ (_previous trailsDB)
                          guard_ (previous_previous_signature previous ==. val_ searchSignature)
                          pure $ (previous_entry_signature previous)

  entries <- traverse getEntryBySignature (entriesPrimaryKeyToSignature <$> followingSignatures)

  followingEntries <- concat <$> traverse getPreviousEntriesQuery (trailEntryResponseSignature <$> entries)

  pure $ followingEntries <> entries


getPreviousEntriesBySignatureQuery :: (AsTrailsServiceError err)
                           => SignaturePlaceholder -> DB context err [TrailEntryResponse]
getPreviousEntriesBySignatureQuery searchSignature = do
  entry <- getEntryBySignature searchSignature
  let previous = trailEntryResponseParentSignatures entry
  previousEntries <- concat <$> traverse getPreviousEntriesBySignatureQuery previous
  pure $ entry : previousEntries


getEntryBySignature :: (AsTrailsServiceError err)
                    => SignaturePlaceholder -> DB context err TrailEntryResponse
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
                    pure $ buildTrailEntryResponse entry previous


buildTrailEntryResponse :: Entries -> [Previous] -> TrailEntryResponse
buildTrailEntryResponse entries previous = TrailEntryResponse 1
                                          (onLocalTime EntryTime $ entries_timestamp entries)
                                          (entries_gs1company_prefix entries)
                                          (EventId $ entries_event_id entries)
                                          (previous_previous_signature <$> previous)
                                          (entries_signature entries)


addTrail  :: ( Member context '[HasEnvType, HasConnPool, HasKatipContext, HasKatipLogEnv]
             , Member err '[AsTrailsServiceError, AsSqlError])
          => [TrailEntryResponse] ->  AppM context err NoContent
addTrail trail = do
  _ <- runDb $ (addEntryQuery trail)
  pure NoContent


addEntryQuery :: (AsTrailsServiceError err)
              => [TrailEntryResponse] -> DB context err ()
addEntryQuery entries_raw = do
  let entries = trailEntryResponseToEntriesT <$> entries_raw
  let previous = concat $ trailEntryResponseToParentsT <$> entries_raw
  _ <- pg $ runInsertReturningList $ insert (_entries trailsDB)
          $ insertValues entries
  _ <- pg $ runInsertReturningList $ insert (_previous trailsDB)
          $ insertValues previous
  pure ()


trailEntryResponseToEntriesT :: TrailEntryResponse -> EntriesT Identity
trailEntryResponseToEntriesT trailEntry = EntriesT (trailEntryResponseSignature trailEntry)
                                           (toDbTimestamp $ getEntryTime $ trailEntryResponseTimestamp trailEntry)
                                           (trailEntryResponseGS1CompanyPrefix trailEntry)
                                           (unEventId $ trailEntryResponseEventID trailEntry)
                                           Nothing


trailEntryResponseToParentsT :: TrailEntryResponse -> [PreviousT Identity]
trailEntryResponseToParentsT trailEntry = (PreviousT (EntriesPrimaryKey $ trailEntryResponseSignature trailEntry)) <$> (trailEntryResponseParentSignatures trailEntry)
