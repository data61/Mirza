{-# LANGUAGE MultiParamTypeClasses #-}

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
import           Mirza.SupplyChain.Types            hiding (User (..))
import qualified Mirza.SupplyChain.Types            as ST

import           Mirza.SupplyChain.Handlers.Queries

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

import           Data.List.NonEmpty                 (nonEmpty, (<|))

insertObjectEvent :: SCSApp context err => ST.User
                  -> ObjectEvent
                  -> AppM context err (EventInfo, Schema.EventId)
insertObjectEvent user ob = runDb $ insertObjectEventQuery user ob

insertObjectEventQuery :: AsServiceError err
                       => ST.User
                       -> ObjectEvent
                       -> DB context err (EventInfo, Schema.EventId)
insertObjectEventQuery
  (ST.User ownerUserId@(ST.UserId tUserId) _ _ )
  (ObjectEvent
    foreignEventId
    act
    labelEpcs
    dwhen dwhy dwhere
  ) = do
  let
      schemaUserId = Schema.UserId tUserId -- converting from model to storage UserId
      dwhat =  ObjWhat $ ObjectDWhat act labelEpcs
      event = Ev.Event Ev.ObjectEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent schemaUserId event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (Schema.WhatId whatId)) labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId (EventOwner ownerUserId) False Nothing (SigningUser ownerUserId)
  mapM_ (insertWhatLabel (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds
  pure (evInfo, eventId)


insertAggEvent :: SCSApp context err => ST.User
               -> AggregationEvent
               -> AppM context err (EventInfo, Schema.EventId)
insertAggEvent user ev = runDb $ insertAggEventQuery user ev

insertAggEventQuery :: AsServiceError err
                    => ST.User
                    -> AggregationEvent
                    -> DB context err (EventInfo, Schema.EventId)
insertAggEventQuery
  (ST.User ownerUserId@(ST.UserId tUserId) _ _ )
  (AggregationEvent
    foreignEventId
    act
    mParentLabel
    labelEpcs
    dwhen dwhy dwhere
  ) = do
  let
      schemaUserId = Schema.UserId tUserId
      dwhat =  AggWhat $ AggregationDWhat act mParentLabel labelEpcs
      event = Ev.Event Ev.AggregationEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent schemaUserId event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (Schema.WhatId whatId)) labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  mapM_ (insertLabel (Just MU.Parent) (Schema.WhatId whatId)) (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId (EventOwner ownerUserId) False Nothing (SigningUser ownerUserId)
  mapM_ (insertWhatLabel (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds
  pure (evInfo, eventId)


insertTransactEvent :: SCSApp context err => ST.User
                    -> TransactionEvent
                    -> AppM context err (EventInfo, Schema.EventId)
insertTransactEvent user ev = runDb $ insertTransactEventQuery user ev

insertTransactEventQuery :: AsServiceError err
                         => ST.User
                         -> TransactionEvent
                         -> DB context err (EventInfo, Schema.EventId)
insertTransactEventQuery
  (ST.User userId@(ST.UserId tUserId) _ _ )
  (TransactionEvent
    foreignEventId
    act
    mParentLabel
    bizTransactions
    labelEpcs
    otherUsers
    dwhen dwhy dwhere
  ) = do
  let
      schemaUserId = Schema.UserId tUserId
      dwhat =  TransactWhat $ TransactionDWhat act mParentLabel bizTransactions labelEpcs
      event = Ev.Event Ev.TransactionEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent schemaUserId event
  let ownerId = EventOwner userId

  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (Schema.WhatId whatId)) labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  mapM_ (insertLabel (Just MU.Parent) (Schema.WhatId whatId)) (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  _r <- sequence $ insertUserEvent eventId ownerId False Nothing <$> SigningUser <$> userId <| otherUsers
  mapM_ (insertWhatLabel (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds

  pure (evInfo, eventId)

insertTransfEvent :: SCSApp context err => ST.User
                  -> TransformationEvent
                  -> AppM context err (EventInfo, Schema.EventId)
insertTransfEvent user ev = runDb $ insertTransfEventQuery user ev

insertTransfEventQuery :: AsServiceError err
                       => ST.User
                       -> TransformationEvent
                       -> DB context err (EventInfo, Schema.EventId)
insertTransfEventQuery
  (ST.User ownerUserId@(ST.UserId tUserId) _ _ )
  (TransformationEvent
    foreignEventId
    mTransfId
    inputs
    outputs
    dwhen dwhy dwhere
  ) = do
  let
      schemaUserId = Schema.UserId tUserId
      dwhat =  TransformWhat $ TransformationDWhat mTransfId inputs outputs
      event = Ev.Event Ev.TransformationEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent schemaUserId event
  whatId <- insertDWhat Nothing dwhat eventId
  inputLabelIds <- mapM (\(InputEPC i) -> insertLabel (Just MU.Input) (Schema.WhatId whatId) i) inputs
  outputLabelIds <- mapM (\(OutputEPC o) -> insertLabel (Just MU.Output) (Schema.WhatId whatId) o) outputs
  let labelIds = Schema.LabelId <$> (inputLabelIds ++ outputLabelIds)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId (EventOwner ownerUserId) False Nothing (SigningUser ownerUserId)
  mapM_ (insertWhatLabel (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds

  pure (evInfo, eventId)


sendToBlockchain :: (SCSApp context err, AsServantError err)
                 => ST.User
                 -> EvId.EventId
                 -> AppM context err (EventBlockchainStatus, Maybe BlockchainId)
sendToBlockchain user eventId = do
  evInfo <- eventInfo user eventId
  let eventStatus = eventInfoBlockChainStatus evInfo
  return $ case eventStatus of
    ReadyAndWaiting -> do
      case (nonEmpty . eventInfoUserSigs $ evInfo) of
        Nothing -> error "it should never get here"
        Just userSigs -> do
              let evToSign = eventToSign evInfo
              let bcPackage = BlockchainPackage evToSign userSigs
              error "not implemented yet"
    _               -> (NeedMoreSignatures, Nothing)
