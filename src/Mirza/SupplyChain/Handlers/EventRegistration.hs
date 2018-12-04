{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}

module Mirza.SupplyChain.Handlers.EventRegistration
  (
    insertAggEvent
  , insertObjectEvent
  , insertTransactEvent
  , insertTransfEvent
  , sendToBlockchain
  ) where

import qualified Mirza.Common.GS1BeamOrphans        as MU
import           Mirza.SupplyChain.Database.Schema  as Schema
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

import           Data.List.NonEmpty                 (nonEmpty, toList, (<|))
import           Data.List.Unique                   (allUnique)

import           Data.Maybe                         (fromJust, isJust, maybe)

import           Control.Monad.Except               (unless, when)

insertObjectEvent :: (Member context '[HasDB],
                      Member err     '[AsSqlError])
                  => ST.User
                  -> ObjectEvent
                  -> AppM context err (EventInfo, Schema.EventId)
insertObjectEvent user ob = runDb $ insertObjectEventQuery user ob

insertObjectEventQuery :: ST.User
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
  labelIds' <- mapM insertLabel labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId (EventOwner ownerUserId) False Nothing (SigningUser ownerUserId)
  mapM_ (insertWhatLabel Nothing (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent eventId) labelIds
  pure (evInfo, eventId)


insertAggEvent  :: (Member context '[HasDB],
                    Member err     '[AsSqlError])
                => ST.User
                -> AggregationEvent
                -> AppM context err (EventInfo, Schema.EventId)
insertAggEvent user ev = runDb $ insertAggEventQuery user ev

insertAggEventQuery :: ST.User
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
  labelIds' <- mapM insertLabel labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  mParentLabelId <- mapM insertLabel (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId (EventOwner ownerUserId) False Nothing (SigningUser ownerUserId)
  mapM_ (insertWhatLabel Nothing (Schema.WhatId whatId)) labelIds
  when (isJust mParentLabelId) $
    insertWhatLabel (Just MU.Parent) (Schema.WhatId whatId) (Schema.LabelId . fromJust $ mParentLabelId) >>= (\_ -> pure ())
  mapM_ (insertLabelEvent eventId) labelIds
  pure (evInfo, eventId)


insertTransactEvent :: (Member context '[HasDB],
                        Member err     '[AsSqlError, AsServiceError])
                    =>  ST.User
                    -> TransactionEvent
                    -> AppM context err (EventInfo, Schema.EventId)
insertTransactEvent user ev = runDb $ insertTransactEventQuery user ev

insertTransactEventQuery :: Member err '[AsServiceError]
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

  unless (allUnique $ toList otherUsers) $ throwing _DuplicateUsers otherUsers

  let
      schemaUserId = Schema.UserId tUserId
      dwhat =  TransactWhat $ TransactionDWhat act mParentLabel bizTransactions labelEpcs
      event = Ev.Event Ev.TransactionEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent schemaUserId event
  let ownerId = EventOwner userId

  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM insertLabel labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  mParentLabelId <- mapM insertLabel (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  _r <- sequence $ insertUserEvent eventId ownerId False Nothing <$> SigningUser <$> userId <| otherUsers
  mapM_ (insertWhatLabel Nothing (Schema.WhatId whatId)) labelIds
  when (isJust mParentLabelId) $
      insertWhatLabel (Just MU.Parent) (Schema.WhatId whatId) (Schema.LabelId . fromJust $ mParentLabelId) >>= (\_ -> pure ())
  mapM_ (insertLabelEvent eventId) labelIds

  pure (evInfo, eventId)


insertTransfEvent :: (Member context '[HasDB],
                      Member err     '[AsSqlError])
                  => ST.User
                  -> TransformationEvent
                  -> AppM context err (EventInfo, Schema.EventId)
insertTransfEvent user ev = runDb $ insertTransfEventQuery user ev

insertTransfEventQuery :: ST.User
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
  inputLabelIds <- mapM (\(InputEPC i) -> insertLabel i) inputs
  outputLabelIds <- mapM (\(OutputEPC o) -> insertLabel o) outputs
  let labelIds = Schema.LabelId <$> (inputLabelIds ++ outputLabelIds)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId (EventOwner ownerUserId) False Nothing (SigningUser ownerUserId)
  mapM_ (insertWhatLabel (Just MU.Input)  (Schema.WhatId whatId) . Schema.LabelId) inputLabelIds
  mapM_ (insertWhatLabel (Just MU.Output) (Schema.WhatId whatId) . Schema.LabelId) outputLabelIds
  mapM_ (insertLabelEvent eventId) labelIds

  pure (evInfo, eventId)


sendToBlockchain  :: (Member context '[HasDB],
                      Member err     '[AsSqlError, AsServiceError])
                  => ST.User
                  -> EvId.EventId
                  -> AppM context err (EventBlockchainStatus, Maybe BlockchainId)
sendToBlockchain user eventId = do
  evInfo <- eventInfo user eventId
  let eventStatus = eventInfoBlockChainStatus evInfo
  return $ case eventStatus of
    ReadyAndWaiting -> do
      (flip $ maybe (error "it should not happen"))
        (nonEmpty . eventInfoUserSigs $ evInfo) $ \userSigs -> do
            let evToSign = eventToSign evInfo
            let _bcPackage = BlockchainPackage evToSign userSigs
            error "not implemented yet"
    _               -> (NeedMoreSignatures, Nothing)
