{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Queries
  (
    epcState
  , listEvents, eventInfo, eventList, eventUserList, eventsByUser
  , eventUserSignedList
  , queryUserId
  ) where


import           Mirza.SupplyChain.Handlers.Common
import           Mirza.SupplyChain.Handlers.EventRegistration (findEvent,
                                                               findLabelId,
                                                               getEventList)
import           Mirza.SupplyChain.Handlers.Users             (userTableToModel)

import qualified Mirza.Common.Utils                           as U
import           Mirza.SupplyChain.ErrorUtils                 (throwParseError)
import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.StorageBeam                as SB
import           Mirza.SupplyChain.Types                      hiding
                                                               (NewUser (..),
                                                               User (..))
import qualified Mirza.SupplyChain.Types                      as ST

import           Data.GS1.DWhat                               (LabelEPC (..),
                                                               urn2LabelEPC)
import qualified Data.GS1.Event                               as Ev
import           Data.GS1.EventId                             as EvId

import           Database.Beam                                as B

import           Data.Bifunctor                               (bimap)
import           Data.Maybe                                   (catMaybes)


-- Use getLabelIdState
epcState :: ST.User ->  LabelEPCUrn -> AppM context err EPCState
epcState _user _str = U.notImplemented


-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
listEvents :: SCSApp context err
           => ST.User
           ->  LabelEPCUrn
           -> AppM context err [Ev.Event]
listEvents _user = either throwParseError (runDb . listEventsQuery) . urn2LabelEPC . getLabelEPCUrn

listEventsQuery :: AsServiceError err => LabelEPC -> DB context err [Ev.Event]
listEventsQuery labelEpc =
  maybe (return []) (getEventList . SB.LabelId) =<< findLabelId labelEpc



eventInfo :: SCSApp context err
          => ST.User
          -> EvId.EventId
          -> AppM context err (Maybe Ev.Event)
eventInfo _user = runDb . findEvent . SB.EventId . EvId.unEventId



-- |List events that a particular user was/is involved with
-- use BizTransactions and events (createdby) tables
eventList :: SCSApp context err
          => ST.User
          -> UserId
          -> AppM context err [Ev.Event]
eventList _user = runDb . eventsByUser

eventsByUser :: ST.UserId -> DB context err [Ev.Event]
eventsByUser (ST.UserId userId) = do
  events <- pg $ runSelectReturningList $ select $ do
    userEvent <- all_ (SB._user_events SB.supplyChainDb)
    event <- all_ (SB._events SB.supplyChainDb)
    guard_ (SB.user_events_event_id userEvent `references_` event &&.
            SB.user_events_user_id userEvent ==. val_ (SB.UserId userId))
    pure (SB.event_json event)
  return $ catMaybes $ decodeEvent <$> events



-- given an event Id, list all the users associated with that event
-- this can be used to make sure everything is signed
eventUserList :: SCSApp context err
              => ST.User
              -> EvId.EventId
              -> AppM context err [(ST.User, Bool)]
eventUserList _user = runDb . eventUserSignedList

-- TODO: Write tests
-- Returns the user and whether or not that user had signed the event
eventUserSignedList :: EvId.EventId -> DB context err [(ST.User, Bool)]
eventUserSignedList (EvId.EventId eventId) = do
  usersSignedList <- pg $ runSelectReturningList $ select $ do
    userEvent <- all_ (SB._user_events SB.supplyChainDb)
    user <- all_ (SB._users SB.supplyChainDb)
    guard_ (SB.user_events_event_id userEvent ==. val_ (SB.EventId eventId))
    guard_ (SB.user_events_user_id userEvent `references_` user)
    pure (user, SB.user_events_has_signed userEvent)
  return $ bimap userTableToModel id <$> usersSignedList

queryUserId :: SCSApp context err => ST.User -> AppM context err UserId
queryUserId = return . ST.userId
