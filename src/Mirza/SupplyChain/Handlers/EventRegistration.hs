{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Mirza.SupplyChain.Handlers.EventRegistration
  (
    insertAggEvent
  , insertObjectEvent
  , insertTransactEvent
  , insertTransfEvent
  ) where

import qualified Mirza.Common.GS1BeamOrphans        as MU
import           Mirza.SupplyChain.Database.Schema  as Schema
import           Mirza.SupplyChain.Handlers.Common
import           Mirza.SupplyChain.Handlers.Queries (eventInfoQuery)
import           Mirza.SupplyChain.Types            hiding (User (..))
import qualified Mirza.SupplyChain.Types            as ST

import           Mirza.SupplyChain.EventUtils

import           Data.GS1.DWhat                     (AggregationDWhat (..),
                                                     DWhat (..), InputEPC (..),
                                                     LabelEPC (..),
                                                     ObjectDWhat (..),
                                                     OutputEPC (..),
                                                     ParentLabel (..),
                                                     TransactionDWhat (..),
                                                     TransformationDWhat (..))
import           Data.GS1.Event                     as Ev
import           Data.GS1.EventId                   as EvId


insertObjectEvent :: SCSApp context err => ST.User
                  -> ObjectEvent
                  -> AppM context err EventInfo
insertObjectEvent user ob = runDb $ insertObjectEventQuery user ob

insertObjectEventQuery :: AsServiceError err
                       => ST.User
                       -> ObjectEvent
                       -> DB context err EventInfo
insertObjectEventQuery
  user@(ST.User (ST.UserId tUserId) _ _ )
  (ObjectEvent
    foreignEventId
    act
    labelEpcs
    dwhen dwhy dwhere
  ) = do

  let
      userId = Schema.UserId tUserId -- converting from model to storage UserId
      dwhat =  ObjWhat $ ObjectDWhat act labelEpcs
      event = Ev.Event Ev.ObjectEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  eventId@(Schema.EventId schemaEventId) <- insertEvent userId event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (Schema.WhatId whatId)) labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds
  eventInfoQuery user (EvId.EventId schemaEventId)


insertAggEvent :: SCSApp context err => ST.User
               -> AggregationEvent
               -> AppM context err EventInfo
insertAggEvent user ev = runDb $ insertAggEventQuery user ev

insertAggEventQuery :: AsServiceError err
                    => ST.User
                    -> AggregationEvent
                    -> DB context err EventInfo
insertAggEventQuery
  user@(ST.User (ST.UserId tUserId) _ _ )
  (AggregationEvent
    foreignEventId
    act
    mParentLabel
    labelEpcs
    dwhen dwhy dwhere
  ) = do
  let
      userId = Schema.UserId tUserId
      dwhat =  AggWhat $ AggregationDWhat act mParentLabel labelEpcs
      event = Ev.Event Ev.AggregationEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  eventId@(Schema.EventId schemaEventId) <- insertEvent userId event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (Schema.WhatId whatId)) labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  mapM_ (insertLabel (Just MU.Parent) (Schema.WhatId whatId)) (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds

  eventInfoQuery user (EvId.EventId schemaEventId)


insertTransactEvent :: SCSApp context err => ST.User
                    -> TransactionEvent
                    -> AppM context err EventInfo
insertTransactEvent user ev = runDb $ insertTransactEventQuery user ev

insertTransactEventQuery :: AsServiceError err
                         => ST.User
                         -> TransactionEvent
                         -> DB context err EventInfo
insertTransactEventQuery
  user@(ST.User (ST.UserId tUserId) _ _ )
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
      userId = Schema.UserId tUserId
      dwhat =  TransactWhat $ TransactionDWhat act mParentLabel bizTransactions labelEpcs
      event = Ev.Event Ev.TransactionEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  eventId@(Schema.EventId schemaEventId) <- insertEvent userId event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (Schema.WhatId whatId)) labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  mapM_ (insertLabel (Just MU.Parent) (Schema.WhatId whatId)) (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds

  eventInfoQuery user (EvId.EventId schemaEventId)


insertTransfEvent :: SCSApp context err => ST.User
                  -> TransformationEvent
                  -> AppM context err EventInfo
insertTransfEvent user ev = runDb $ insertTransfEventQuery user ev

insertTransfEventQuery :: AsServiceError err
                    => ST.User
                       -> TransformationEvent
                       -> DB context err EventInfo
insertTransfEventQuery
  user@(ST.User (ST.UserId tUserId) _ _ )
  (TransformationEvent
    foreignEventId
    mTransfId
    inputs
    outputs
    dwhen dwhy dwhere
  ) = do
  let
      userId = Schema.UserId tUserId
      dwhat =  TransformWhat $ TransformationDWhat mTransfId inputs outputs
      event = Ev.Event Ev.TransformationEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  eventId@(Schema.EventId schemaEventId) <- insertEvent userId event
  whatId <- insertDWhat Nothing dwhat eventId
  inputLabelIds <- mapM (\(InputEPC i) -> insertLabel (Just MU.Input) (Schema.WhatId whatId) i) inputs
  outputLabelIds <- mapM (\(OutputEPC o) -> insertLabel (Just MU.Output) (Schema.WhatId whatId) o) outputs
  let labelIds = Schema.LabelId <$> (inputLabelIds ++ outputLabelIds)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds

  eventInfoQuery user (EvId.EventId schemaEventId)
