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
  parentEntries <- getParentsBySignatureQuery searchSignature
  childEntries <- getChildrenQuery searchSignature
  pure $ childEntries <> parentEntries


getChildrenQuery :: (AsTrailsServiceError err)
                => SignaturePlaceholder -> DB context err [TrailEntryResponse]
getChildrenQuery searchSignature = do
  childSignatures <- pg $ runSelectReturningList $ select $ do
                          parents <- all_ (_parents trailsDB)
                          guard_ (parents_parent_signature parents ==. val_ searchSignature)
                          pure $ (parents_entry_signature parents)

  entries <- traverse getEntryBySignature (entriesPrimaryKeyToSignature <$> childSignatures)

  childEntries <- concat <$> traverse getChildrenQuery (trailEntryResponseSignature <$> entries)

  pure $ childEntries <> entries


getParentsBySignatureQuery :: (AsTrailsServiceError err)
                           => SignaturePlaceholder -> DB context err [TrailEntryResponse]
getParentsBySignatureQuery searchSignature = do
  entry <- getEntryBySignature searchSignature
  let parents = trailEntryResponseParentSignatures entry
  parentEntries <- concat <$> traverse getParentsBySignatureQuery parents
  pure $ entry : parentEntries


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
                    parents <- pg $ runSelectReturningList $ select $ do
                                 parents <- all_ (_parents trailsDB)
                                 guard_ (parents_entry_signature parents ==. val_ (EntriesPrimaryKey searchSignature))
                                 pure parents
                    pure $ buildTrailEntryResponse entry parents


buildTrailEntryResponse :: Entries -> [Parents] -> TrailEntryResponse
buildTrailEntryResponse entries parents = TrailEntryResponse 1
                                          (onLocalTime EntryTime $ entries_timestamp entries)
                                          (entries_gs1company_prefix entries)
                                          (EventId $ entries_event_id entries)
                                          (parents_parent_signature <$> parents)
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
  let parents = concat $ trailEntryResponseToParentsT <$> entries_raw
  _ <- pg $ runInsertReturningList $ insert (_entries trailsDB)
          $ insertValues entries
  _ <- pg $ runInsertReturningList $ insert (_parents trailsDB)
          $ insertValues parents
  pure ()


trailEntryResponseToEntriesT :: TrailEntryResponse -> EntriesT Identity
trailEntryResponseToEntriesT trailEntry = EntriesT (trailEntryResponseSignature trailEntry)
                                           (toDbTimestamp $ getEntryTime $ trailEntryResponseTimestamp trailEntry)
                                           (trailEntryResponseGS1CompanyPrefix trailEntry)
                                           (unEventId $ trailEntryResponseEventID trailEntry)
                                           Nothing


trailEntryResponseToParentsT :: TrailEntryResponse -> [ParentsT Identity]
trailEntryResponseToParentsT trailEntry = (ParentsT (EntriesPrimaryKey $ trailEntryResponseSignature trailEntry)) <$> (trailEntryResponseParentSignatures trailEntry)
