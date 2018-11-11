{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Queries
  ( listEvents, eventInfo, eventInfoQuery, eventList, eventUserList, eventsByUser
  , eventUserSignedList
  , queryUserId
  ) where


import           Mirza.Common.Utils                    (fromPgJSON)
import           Mirza.SupplyChain.EventUtils          (findLabelId,
                                                        findSchemaEvent,
                                                        getEventList)
import           Mirza.SupplyChain.Handlers.Common
import           Mirza.SupplyChain.Handlers.Signatures
import           Mirza.SupplyChain.Handlers.Users      (userTableToModel)

import           Mirza.SupplyChain.Database.Schema     as Schema
import           Mirza.SupplyChain.ErrorUtils          (throwParseError)
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.Types               hiding (NewUser (..),
                                                        User (..))
import qualified Mirza.SupplyChain.Types               as ST

import           Data.GS1.DWhat                        (LabelEPC (..),
                                                        urn2LabelEPC)
import qualified Data.GS1.Event                        as Ev
import           Data.GS1.EventId                      as EvId

import           Database.Beam                         as B

import           Control.Lens                          (( # ))
import           Control.Monad.Error.Hoist

import           Data.Bifunctor                        (bimap)

import           Crypto.JOSE.Types                     (Base64Octets (..))


-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
listEvents :: SCSApp context err
           => ST.User
           -> LabelEPCUrn
           -> AppM context err [Ev.Event]
listEvents _user = either throwParseError (runDb . listEventsQuery) . urn2LabelEPC . getLabelEPCUrn

listEventsQuery :: AsServiceError err => LabelEPC -> DB context err [Ev.Event]
listEventsQuery labelEpc =
  maybe (pure []) (getEventList . Schema.LabelId) =<< findLabelId labelEpc


eventInfo :: (SCSApp context err, AsServantError err)
          => ST.User
          -> EvId.EventId
          -> AppM context err EventInfo
eventInfo user eventId = runDb $ eventInfoQuery user eventId

eventInfoQuery :: AsServiceError err
               => ST.User
               -> EvId.EventId
               -> DB context err EventInfo
eventInfoQuery _user eventId@(EvId.EventId eId) = do
  usersWithEvent <- eventUserSignedList eventId
  schemaEvent <- findSchemaEvent (Schema.EventId eId) <!?> (_InvalidEventId # eventId)
  let event = storageToModelEvent schemaEvent
      unsignedUserIds = map (ST.userId . fst) $ filter (not . snd) usersWithEvent
      signedUserIds = (ST.userId . fst) <$> filter snd usersWithEvent
  signedEvents <- mapM (flip findSignedEventByUser eventId) signedUserIds
  let usersAndSignedEvents = zip signedUserIds signedEvents
  pure $ EventInfo event usersAndSignedEvents unsignedUserIds
                  (Base64Octets $ event_to_sign schemaEvent) NotSent


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
