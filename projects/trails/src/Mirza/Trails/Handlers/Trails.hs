{-# LANGUAGE OverloadedStrings #-}

module Mirza.Trails.Handlers.Trails where

import           Mirza.Trails.Types

import           Mirza.Common.Types

import           Data.GS1.EPC       (GS1CompanyPrefix (..))
import           Data.GS1.EventId   (EventId (..))

import           Data.Time.Clock
import           Data.UUID.V4

import           Servant


getTrailByEventId :: EventId ->  AppM context err [TrailEntryResponse]
getTrailByEventId (EventId uuid) = do
  time <- liftIO getCurrentTime
  --uuid1 <- liftIO nextRandom
  pure $ [ TrailEntryResponse 1
                (EntryTime time)
                (GS1CompanyPrefix "0000001")
                (EventId uuid)
                []
                (SignaturePlaceholder "TODO")
         ]


getTrailBySignature :: SignaturePlaceholder ->  AppM context err [TrailEntryResponse]
getTrailBySignature (SignaturePlaceholder sig) = do
       time <- liftIO getCurrentTime
       uuid <- liftIO nextRandom
       pure $ [ TrailEntryResponse 1
                     (EntryTime time)
                     (GS1CompanyPrefix "0000002")
                     (EventId uuid)
                     []
                     (SignaturePlaceholder sig)
              ]


addTrail :: [TrailEntryResponse] ->  AppM context err NoContent
addTrail _ = pure NoContent
