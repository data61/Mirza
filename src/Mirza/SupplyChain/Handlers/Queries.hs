{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Queries
  ( listEvents, eventInfo, eventInfoQuery, eventList, eventUserList, eventsByUser
  , eventUserSignedList
  , queryUserId
  , findSignatureByEvent, findSignedEventByEvent
  ) where


import           Mirza.Common.Utils                (fromPgJSON)
import           Mirza.SupplyChain.EventUtils      (findLabelId,
                                                    findSchemaEvent,
                                                    getEventList)
import           Mirza.SupplyChain.Handlers.Common
import           Mirza.SupplyChain.Handlers.Users  (userTableToModel)

import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.ErrorUtils      (throwBackendError,
                                                    throwParseError)
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.Types           hiding (NewUser (..),
                                                    User (..))
import qualified Mirza.SupplyChain.Types           as ST

import           Data.GS1.DWhat                    (LabelEPC (..), urn2LabelEPC)
import qualified Data.GS1.Event                    as Ev
import           Data.GS1.EventId                  as EvId

import           Database.Beam                     as B

import           Control.Lens                      (( # ))
import           Control.Monad.Error.Hoist

import           Data.Bifunctor                    (bimap)

import           Crypto.JOSE.Types                 (Base64Octets (..))

import           Data.List                         (partition)

import           Database.Beam.Postgres            (PgJSON (..))

-- This takes an EPC urn,
-- and looks up all the events related to that item.
listEvents :: SCSApp context err
           => ST.User
           -> LabelEPCUrn
           -> AppM context err [Ev.Event]
listEvents _user = either throwParseError (runDb . listEventsQuery) . urn2LabelEPC . getLabelEPCUrn

listEventsQuery :: AsServiceError err => LabelEPC -> DB context err [Ev.Event]
listEventsQuery labelEpc = do
  labelIds <- findLabelId labelEpc
  allEvents <- traverse (getEventList . Schema.LabelId) labelIds
  pure $ concat allEvents

eventInfo :: (SCSApp context err, AsServantError err)
          => ST.User
          -> EvId.EventId
          -> AppM context err EventInfo
eventInfo _user eventId = runDb $ eventInfoQuery eventId

eventInfoQuery :: AsServiceError err
               => EvId.EventId
               -> DB context err EventInfo
eventInfoQuery eventId@(EvId.EventId eId) = do
  usersWithEvent <- eventUserSignedList eventId
  schemaEvent <- findSchemaEvent (Schema.EventId eId) <!?> (_InvalidEventId # eventId)
  let event = storageToModelEvent schemaEvent
      (signedUserIds, unsignedUserIds) =
          bimap
            (map (ST.userId . fst))
            (map (ST.userId . fst)) $
            partition snd usersWithEvent
  signedEvents <- mapM (`findSignedEventByUser` eventId) signedUserIds
  let usersAndSignedEvents = zip signedUserIds signedEvents
  let eventStatus = if null unsignedUserIds then ReadyAndWaiting else NeedMoreSignatures
  pure $ EventInfo event usersAndSignedEvents unsignedUserIds
                  (Base64Octets $ event_to_sign schemaEvent) eventStatus


-- |List events that a particular user was/is involved with
-- use BizTransactions and events (createdby) tables
eventList :: SCSApp context err
          => ST.User
          -> ST.UserId
          -> AppM context err [Ev.Event]
eventList _user = runDb . eventsByUser

eventsByUser :: ST.UserId -> DB context err [Ev.Event]
eventsByUser (ST.UserId userId) = do
  events <- pg $ runSelectReturningList $ select $ do
    userEvent <- all_ (Schema._user_events Schema.supplyChainDb)
    event <- all_ (Schema._events Schema.supplyChainDb)
    guard_ (Schema.user_events_event_id userEvent `references_` event &&.
            Schema.user_events_user_id userEvent ==. val_ (Schema.UserId userId))
    pure (Schema.event_json event)
  pure $ fromPgJSON <$> events

-- | Given an eventId, list all the users associated with that event
-- This can be used to make sure everything is signed
eventUserList :: SCSApp context err
              => ST.User
              -> EvId.EventId
              -> AppM context err [(ST.User, Bool)]
eventUserList _user = runDb . eventUserSignedList

-- TODO: Write tests
-- Returns all the users related to the event
-- and whether or not that user had signed the event
eventUserSignedList :: EvId.EventId -> DB context err [(ST.User, Bool)]
eventUserSignedList (EvId.EventId eventId) = do
  usersSignedList <- pg $ runSelectReturningList $ select $ do
    userEvent <- all_ (Schema._user_events Schema.supplyChainDb)
    guard_ (Schema.user_events_event_id userEvent ==. val_ (Schema.EventId eventId))
    user <- related_ (Schema._users Schema.supplyChainDb) (Schema.user_events_user_id userEvent)
    pure (user, Schema.user_events_has_signed userEvent)
  pure $ bimap userTableToModel id <$> usersSignedList

queryUserId :: SCSApp context err => ST.User -> AppM context err ST.UserId
queryUserId = pure . ST.userId

findSignatureByEvent :: (AsServiceError err)
                     => EvId.EventId
                     -> DB context err [Schema.Signature]
findSignatureByEvent (EvId.EventId eId) =
  pg $ runSelectReturningList $ select $ do
    sig <- all_ (Schema._signatures Schema.supplyChainDb)
    guard_ ((Schema.signature_event_id sig) ==. (val_ (Schema.EventId eId)))
    pure sig

findSignedEventByEvent :: (AsServiceError err)
                       => EvId.EventId
                       -> DB context err [ST.SignedEvent]
findSignedEventByEvent eventId = fmap signatureToSignedEvent <$> findSignatureByEvent eventId

findSignatureByUser :: AsServiceError err
                    => ST.UserId
                    -> EvId.EventId
                    -> DB context err Schema.Signature
findSignatureByUser (ST.UserId uId) (EvId.EventId eId) = do
  r <- pg $ runSelectReturningList $ select $ do
    sig <- all_ (Schema._signatures Schema.supplyChainDb)
    guard_ ((Schema.signature_event_id sig) ==. (val_ (Schema.EventId eId)) &&.
            (Schema.signature_user_id sig) ==. (val_ (Schema.UserId uId)))
    pure sig
  case r of
    [sig] -> pure sig
    _     -> throwBackendError ("Invalid User - Event pair" :: String) -- TODO: wrong error to throw here

findSignedEventByUser :: AsServiceError err
                      => ST.UserId
                      -> EvId.EventId
                      -> DB context err ST.SignedEvent
findSignedEventByUser uId eventId = signatureToSignedEvent <$> (findSignatureByUser uId eventId)

signatureToSignedEvent :: Schema.Signature -> ST.SignedEvent
signatureToSignedEvent (Schema.Signature _ _userId _sigId (Schema.EventId eId) brKeyId (PgJSON sig) _)
  = ST.SignedEvent (EvId.EventId eId) brKeyId sig

