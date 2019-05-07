{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.EventRegistration
  ( insertAggEvent
  , insertObjectEvent
  , insertTransactEvent
  , insertTransfEvent
  -- , sendToBlockchain
  ) where

import qualified Mirza.Common.GS1BeamOrphans       as MU
import           Mirza.SupplyChain.Database.Schema as Schema

import           Mirza.SupplyChain.EventUtils
import           Mirza.SupplyChain.Types

import           Data.GS1.DWhat                    (AggregationDWhat (..),
                                                    DWhat (..), InputEPC (..),
                                                    LabelEPC (..),
                                                    ObjectDWhat (..),
                                                    OutputEPC (..),
                                                    ParentLabel (..),
                                                    TransactionDWhat (..),
                                                    TransformationDWhat (..))
import           Data.GS1.Event                    as Ev

import           Data.Maybe                        (isJust)

import           Control.Monad.Except              (when)

insertObjectEvent :: (Member context '[HasDB],
                      Member err     '[AsSqlError])
                  => ObjectEvent
                  -> AppM context err (EventInfo, Schema.EventId)
insertObjectEvent ob = runDb $ insertObjectEventQuery ob

insertObjectEventQuery :: ObjectEvent
                       -> DB context err (EventInfo, Schema.EventId)
insertObjectEventQuery
  (ObjectEvent
    foreignEventId
    act
    labelEpcs
    dwhen dwhy dwhere
  ) = do
  let
      dwhat =  ObjWhat $ ObjectDWhat act labelEpcs
      event = Ev.Event Ev.ObjectEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM insertLabel labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  mapM_ (insertWhatLabel Nothing (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent Nothing eventId) labelIds
  pure (evInfo, eventId)


insertAggEvent  :: (Member context '[HasDB],
                    Member err     '[AsSqlError])
                => AggregationEvent
                -> AppM context err (EventInfo, Schema.EventId)
insertAggEvent ev = runDb $ insertAggEventQuery ev

insertAggEventQuery :: AggregationEvent
                    -> DB context err (EventInfo, Schema.EventId)
insertAggEventQuery
  (AggregationEvent
    foreignEventId
    act
    mParentLabel
    labelEpcs
    dwhen dwhy dwhere
  ) = do
  let
      dwhat =  AggWhat $ AggregationDWhat act mParentLabel labelEpcs
      event = Ev.Event Ev.AggregationEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM insertLabel labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  mParentLabelId <- mapM insertLabel (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  mapM_ (insertWhatLabel Nothing (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent Nothing eventId) labelIds
  when (isJust mParentLabelId) $ do
    let Just parentLabelId' = mParentLabelId
        parentLabelId = Schema.LabelId parentLabelId'
    _ <- insertWhatLabel (Just MU.Parent) (Schema.WhatId whatId) parentLabelId
    _ <- insertLabelEvent (Just MU.Parent) eventId parentLabelId
    pure ()
  pure (evInfo, eventId)


insertTransactEvent :: (Member context '[HasDB],
                        Member err     '[AsSqlError, AsServiceError])
                    => TransactionEvent
                    -> AppM context err (EventInfo, Schema.EventId)
insertTransactEvent ev = runDb $ insertTransactEventQuery ev

insertTransactEventQuery :: Member err '[AsServiceError]
                         => TransactionEvent
                         -> DB context err (EventInfo, Schema.EventId)
insertTransactEventQuery
  (TransactionEvent
    foreignEventId
    act
    mParentLabel
    bizTransactions
    labelEpcs
    dwhen dwhy dwhere
  ) = do

  let
      dwhat =  TransactWhat $ TransactionDWhat act mParentLabel bizTransactions labelEpcs
      event = Ev.Event Ev.TransactionEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM insertLabel labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
  mParentLabelId <- mapM insertLabel (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  mapM_ (insertWhatLabel Nothing (Schema.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent Nothing eventId) labelIds
  when (isJust mParentLabelId) $ do
    let Just parentLabelId' = mParentLabelId
        parentLabelId = Schema.LabelId parentLabelId'
    _ <- insertWhatLabel (Just MU.Parent) (Schema.WhatId whatId) parentLabelId
    _ <- insertLabelEvent (Just MU.Parent) eventId parentLabelId
    pure ()
  pure (evInfo, eventId)


insertTransfEvent :: (Member context '[HasDB],
                      Member err     '[AsSqlError])
                  => TransformationEvent
                  -> AppM context err (EventInfo, Schema.EventId)
insertTransfEvent ev = runDb $ insertTransfEventQuery ev

insertTransfEventQuery :: TransformationEvent
                       -> DB context err (EventInfo, Schema.EventId)
insertTransfEventQuery
  (TransformationEvent
    foreignEventId
    mTransfId
    inputs
    outputs
    dwhen dwhy dwhere
  ) = do
  let
      dwhat =  TransformWhat $ TransformationDWhat mTransfId inputs outputs
      event = Ev.Event Ev.TransformationEventT foreignEventId dwhat dwhen dwhy dwhere

  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  inputLabelIds <- mapM (\(InputEPC i) -> insertLabel i) inputs
  outputLabelIds <- mapM (\(OutputEPC o) -> insertLabel o) outputs
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  mapM_ (insertWhatLabel (Just MU.Input)  (Schema.WhatId whatId) . Schema.LabelId) inputLabelIds
  mapM_ ((insertLabelEvent (Just MU.Input) eventId) . Schema.LabelId) inputLabelIds
  mapM_ (insertWhatLabel (Just MU.Output) (Schema.WhatId whatId) . Schema.LabelId) outputLabelIds
  mapM_ ((insertLabelEvent (Just MU.Output) eventId) . Schema.LabelId) outputLabelIds

  pure (evInfo, eventId)


-- sendToBlockchain  :: (Member context '[HasDB],
--                       Member err     '[AsSqlError, AsServiceError])
--                   => EvId.EventId
--                   -> AppM context err (EventBlockchainStatus, Maybe BlockchainId)
-- sendToBlockchain eventId = do
--   evInfo <- eventInfo eventId
--   let eventStatus = eventInfoBlockChainStatus evInfo
--   return $ case eventStatus of
--     ReadyAndWaiting ->
--       (flip $ maybe (error "it should not happen"))
--         (nonEmpty . eventInfoUserSigs $ evInfo) $ \userSigs -> do
--             let evToSign = eventToSign evInfo
--             let _bcPackage = BlockchainPackage evToSign userSigs
--             error "not implemented yet"
--     _               -> (NeedMoreSignatures, Nothing)
