{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.EventRegistration
  (
    insertAggEvent
  , insertObjectEvent
  , insertTransactEvent
  , insertTransfEvent
  ) where

import           Mirza.SupplyChain.Handlers.Common

import qualified Mirza.SupplyChain.MigrateUtils    as MU
import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.StorageBeam     as SB
import           Mirza.SupplyChain.Types           hiding (KeyInfo (..),
                                                    NewUser (..), User (userId),
                                                    UserID)
import qualified Mirza.SupplyChain.Types           as ST

import           Data.GS1.DWhat                    (AggregationDWhat (..),
                                                    DWhat (..), InputEPC (..),
                                                    LabelEPC (..),
                                                    ObjectDWhat (..),
                                                    OutputEPC (..),
                                                    TransactionDWhat (..),
                                                    TransformationDWhat (..),
                                                    unParentLabel)
import qualified Data.GS1.Event                    as Ev



insertObjectEvent :: SCSApp context err => ST.User -> ObjectEvent -> AppM context err (Ev.Event, SB.EventId)
insertObjectEvent user ob = runDb $ insertObjectEventQuery user ob

insertObjectEventQuery :: ST.User
                  -> ObjectEvent
                  -> DB context err (Ev.Event, SB.EventId)
insertObjectEventQuery
  (ST.User (ST.UserID userId) _ _ )
  (ObjectEvent
    foreignEventId
    act
    labelEpcs
    dwhen dwhy dwhere
  ) = do

  let
      eventType = Ev.ObjectEventT
      dwhat =  ObjWhat $ ObjectDWhat act labelEpcs
      event = Ev.Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (SB.WhatId whatId)) labelEpcs
  let labelIds = SB.LabelId <$> labelIds'
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (SB.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent (SB.EventId eventId)) labelIds

  return (event, (SB.EventId eventId))



insertAggEvent :: SCSApp context err => ST.User -> AggregationEvent -> AppM context err (Ev.Event, SB.EventId)
insertAggEvent user ev = runDb $ insertAggEventQuery user ev

insertAggEventQuery :: ST.User
               -> AggregationEvent
               -> DB context err (Ev.Event, SB.EventId)
insertAggEventQuery
  (ST.User (ST.UserID userId) _ _ )
  (AggregationEvent
    foreignEventId
    act
    mParentLabel
    labelEpcs
    dwhen dwhy dwhere
  ) = do
  let
      eventType = Ev.AggregationEventT
      dwhat =  AggWhat $ AggregationDWhat act mParentLabel labelEpcs
      event = Ev.Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (SB.WhatId whatId)) labelEpcs
  let labelIds = SB.LabelId <$> labelIds'
  mapM_ (insertLabel (Just MU.Parent) (SB.WhatId whatId)) (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (SB.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent (SB.EventId eventId)) labelIds

  -- FIXME: This should return the event as it has been inserted - the user has
  -- no idea what the ID for the transaction is so can't query it later.
  return (event, (SB.EventId eventId))


insertTransactEvent :: SCSApp context err => ST.User -> TransactionEvent -> AppM context err (Ev.Event, SB.EventId)
insertTransactEvent user ev = runDb $ insertTransactEventQuery user ev

insertTransactEventQuery :: ST.User
                    -> TransactionEvent
                    -> DB context err (Ev.Event, SB.EventId)
insertTransactEventQuery
  (ST.User (ST.UserID userId) _ _ )
  (TransactionEvent
    foreignEventId
    act
    mParentLabel
    bizTransactions
    labelEpcs
    _users
    dwhen dwhy dwhere
  ) = do
  let
      eventType = Ev.TransactionEventT
      dwhat =  TransactWhat $ TransactionDWhat act mParentLabel bizTransactions labelEpcs
      event = Ev.Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (SB.WhatId whatId)) labelEpcs
  let labelIds = SB.LabelId <$> labelIds'
  mapM_ (insertLabel (Just MU.Parent) (SB.WhatId whatId)) (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (SB.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent (SB.EventId eventId)) labelIds

  return (event, (SB.EventId eventId))



insertTransfEvent :: SCSApp context err => ST.User -> TransformationEvent -> AppM context err (Ev.Event, SB.EventId)
insertTransfEvent user ev = runDb $ insertTransfEventQuery user ev

insertTransfEventQuery :: ST.User
                  -> TransformationEvent
                  -> DB context err (Ev.Event, SB.EventId)
insertTransfEventQuery
  (ST.User (ST.UserID userId) _ _ )
  (TransformationEvent
    foreignEventId
    mTransfId
    inputs
    outputs
    dwhen dwhy dwhere
  ) = do
  let
      eventType = Ev.TransformationEventT
      dwhat =  TransformWhat $ TransformationDWhat mTransfId inputs outputs
      event = Ev.Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  inputLabelIds <- mapM (\(InputEPC i) -> insertLabel (Just MU.Input) (SB.WhatId whatId) i) inputs
  outputLabelIds <- mapM (\(OutputEPC o) -> insertLabel (Just MU.Output) (SB.WhatId whatId) o) outputs
  let labelIds = SB.LabelId <$> (inputLabelIds ++ outputLabelIds)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (SB.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent (SB.EventId eventId)) labelIds

  return (event, (SB.EventId eventId))
