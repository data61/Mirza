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

import           Mirza.Blockchain.Client.Servant    (addEvent)
import           Mirza.Blockchain.Types             (RegisterEvent(..))

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

import           Crypto.Hash                        (SHA256(..), hashWith)
import           Data.Maybe                         (isJust, maybe)
import           Control.Lens                       (view)
import           Control.Monad.Except               (unless, when)
import           Servant.Client                     (runClientM)

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
  mapM_ (insertLabelEvent Nothing eventId) labelIds
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
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId (EventOwner ownerUserId) False Nothing (SigningUser ownerUserId)
  mapM_ (insertWhatLabel (Just MU.Input)  (Schema.WhatId whatId) . Schema.LabelId) inputLabelIds
  mapM_ ((insertLabelEvent (Just MU.Input) eventId) . Schema.LabelId) inputLabelIds
  mapM_ (insertWhatLabel (Just MU.Output) (Schema.WhatId whatId) . Schema.LabelId) outputLabelIds
  mapM_ ((insertLabelEvent (Just MU.Output) eventId) . Schema.LabelId) outputLabelIds

  pure (evInfo, eventId)


sendToBlockchain  :: (Member context '[HasDB, HasBlockchainClientEnv],
                      Member err     '[AsSqlError, AsServiceError, AsServantError])
                  => ST.User
                  -> EvId.EventId
                  -> AppM context err EventBlockchainStatus
sendToBlockchain user eventId = do
  bcEnv <- view blockchainClientEnv
  case bcEnv of
    Nothing -> pure ReadyAndWaiting
    Just env -> do
      evInfo <- eventInfo user eventId
  
      case eventInfoBlockChainStatus evInfo of
        ReadyAndWaiting -> do
          bcPkg <- maybe
            (error "No sigs")
            (pure . BlockchainPackage (eventInfoEvent evInfo))
            (nonEmpty $ eventInfoUserSigs evInfo)
          result <- liftIO $ do
            let reqB = RegisterEvent <$> hashWith SHA256 (toCanonicalJSON bcPkg)
                                     <*> fmap Just (hashWith SHA256 _)
            either (throwing _ServantError) pure =<< runClientM (addEvent reqB) env
          undefined result
          
        
        x -> pure x
    
    
