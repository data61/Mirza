{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Queries
  ( listEvents, listEventsQuery
  , eventInfo, eventInfoQuery
  , findSignatureByEvent, findSignedEventByEvent
  ) where


import           Mirza.Common.GS1BeamOrphans       (LabelEPCUrn (..))
import           Mirza.SupplyChain.EventUtils      (findLabelId,
                                                    findSchemaEvent,
                                                    getEventList)

import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.ErrorUtils      (throwParseError)
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.Types
import qualified Mirza.SupplyChain.Types           as ST

import           Data.GS1.DWhat                    (LabelEPC (..), urn2LabelEPC)
import qualified Data.GS1.Event                    as Ev
import           Data.GS1.EventId                  as EvId

import           Database.Beam                     as B

import           Control.Lens                      (( # ))
import           Control.Monad.Error.Hoist

import           Crypto.JOSE.Types                 (Base64Octets (..))

import           Database.Beam.Postgres            (PgJSON (..))

-- This takes an EPC urn,
-- and looks up all the events related to that item.
listEvents  :: (Member context '[HasDB],
                Member err     '[AsServiceError, AsSqlError])
            => LabelEPCUrn
            -> AppM context err [Ev.Event]
listEvents = either throwParseError (runDb . listEventsQuery) . urn2LabelEPC . getLabelEPCUrn

listEventsQuery :: Member err '[AsServiceError]
                => LabelEPC
                -> DB context err [Ev.Event]
listEventsQuery labelEpc = do
  labelIds <- findLabelId labelEpc
  allEvents <- traverse (getEventList . Schema.LabelId) labelIds
  pure $ concat allEvents

eventInfo :: (Member context '[HasDB],
              Member err     '[AsSqlError, AsServiceError])
          => EvId.EventId
          -> AppM context err EventInfo
eventInfo eventId = runDb $ eventInfoQuery eventId

eventInfoQuery :: Member err '[AsServiceError, AsSqlError]
               => EvId.EventId
               -> DB context err EventInfo
eventInfoQuery eventId@(EvId.EventId eId) = do
  schemaEvent <- findSchemaEvent (Schema.EventId eId) <!?> (_InvalidEventId # eventId)
  let event = storageToModelEvent schemaEvent
      eventStatus = undefined -- if null unsignedUserIds then ReadyAndWaiting else NeedMoreSignatures
  pure $ EventInfo event (Base64Octets $ event_to_sign schemaEvent) eventStatus

findSignatureByEvent :: EvId.EventId
                     -> DB context err [Schema.Signature]
findSignatureByEvent (EvId.EventId eId) =
  pg $ runSelectReturningList $ select $ do
    sig <- all_ (Schema._signatures Schema.supplyChainDb)
    guard_ ((Schema.signature_event_id sig) ==. (val_ (Schema.EventId eId)))
    pure sig

findSignedEventByEvent :: EvId.EventId
                       -> DB context err [ST.SignedEvent]
findSignedEventByEvent eventId = fmap signatureToSignedEvent <$> findSignatureByEvent eventId

signatureToSignedEvent :: Schema.Signature -> ST.SignedEvent
signatureToSignedEvent (Schema.Signature _ _sigId (Schema.EventId eId) brKeyId (PgJSON sig) _)
  = ST.SignedEvent (EvId.EventId eId) brKeyId sig
