{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Queries
  (
    epcState
  , listEvents, eventInfo, eventList, eventUserList, eventsByUser
  , eventUserSignedList
  ) where



import           Mirza.SupplyChain.Handlers.Common

import           Mirza.SupplyChain.ErrorUtils      (throwParseError)
import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.StorageBeam     as SB
import           Mirza.SupplyChain.Types           hiding (KeyInfo (..),
                                                    NewUser (..), User (..))
import qualified Mirza.SupplyChain.Types           as ST
import qualified Mirza.SupplyChain.Utils           as U

import           Data.GS1.DWhat                    (LabelEPC (..), urn2LabelEPC)
import qualified Data.GS1.Event                    as Ev
import           Data.GS1.EventId                  as EvId

import           Database.Beam                     as B

import           Data.Bifunctor                    (bimap)
import           Data.Maybe                        (catMaybes)



-- PSUEDO:
-- Use getLabelIDState
epcState :: ST.User ->  LabelEPCUrn -> AppM context err EPCState
epcState _user _str = U.notImplemented



-- This takes an EPC urn,
-- and looks up all the events related to that item. First we've got
-- to find all the related "Whats"
-- PSEUDO:
-- (labelID, _) <- getLabelIDState
-- wholeEvents <- select * from events, dwhats, dwhy, dwhen where _whatItemID=labelID AND _eventID=_whatEventID AND _eventID=_whenEventID AND _eventID=_whyEventID ORDER BY _eventTime;
-- return map constructEvent wholeEvents
listEvents ::  SCSApp context err => ST.User ->  LabelEPCUrn -> AppM context err [Ev.Event]
listEvents _user = either throwParseError (runDb . listEventsQuery) . urn2LabelEPC . unLabelEPCUrn

listEventsQuery :: AsServiceError err => LabelEPC -> DB context err [Ev.Event]
listEventsQuery labelEpc =
  maybe (return []) (getEventList . SB.LabelId) =<< findLabelId labelEpc



eventInfo :: SCSApp context err => ST.User -> EvId.EventId -> AppM context err (Maybe Ev.Event)
eventInfo _user = runDb . findEvent . SB.EventId . EvId.unEventId



-- |List events that a particular user was/is involved with
-- use BizTransactions and events (createdby) tables
eventList :: SCSApp context err => ST.User -> UserID -> AppM context err [Ev.Event]
eventList _user = runDb . eventsByUser

eventsByUser :: ST.UserID -> DB context err [Ev.Event]
eventsByUser (ST.UserID userId) = do
  events <- pg $ runSelectReturningList $ select $ do
    userEvent <- all_ (SB._user_events SB.supplyChainDb)
    event <- all_ (SB._events SB.supplyChainDb)
    guard_ (SB.user_events_event_id userEvent `references_` event &&.
            SB.user_events_user_id userEvent ==. val_ (SB.UserId userId))
    pure (SB.json_event event)
  return $ catMaybes $ decodeEvent <$> events



-- given an event ID, list all the users associated with that event
-- this can be used to make sure everything is signed
-- PSEUDO:
-- SELECT event.userID, userID1, userID2 FROM Events, BizTransactions WHERE Events._eventID=eventID AND BizTransactionsEventId=Events._eventID;
-- implement a function constructEvent :: WholeEvent -> Event

-- Look into usereventsT and tie that back to the user
-- the function getUser/selectUser might be helpful
eventUserList :: SCSApp context err => ST.User -> EvId.EventId -> AppM context err [(ST.User, Bool)]
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
