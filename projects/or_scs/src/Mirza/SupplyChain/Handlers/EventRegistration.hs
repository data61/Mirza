{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.EventRegistration
  ( insertGS1Event
  -- , sendToBlockchain
  ) where

import           Mirza.SupplyChain.Database.Schema as Schema

import           Mirza.SupplyChain.EventUtils
import           Mirza.SupplyChain.QueryUtils      (handleError)
import           Mirza.SupplyChain.SqlUtils        (transformSqlUniqueViloation)
import           Mirza.SupplyChain.Types

import           Control.Lens                      (( # ))

import           Data.GS1.Event                    as Ev

insertGS1Event  :: (Member context '[HasDB],
                    Member err     '[AsSqlError, AsServiceError])
                => Ev.Event
                -> AppM context err (EventInfo, Schema.EventId)
insertGS1Event ev = do
  handleError
    (transformSqlUniqueViloation
      "events_event_to_sign_key"
      (const $ _EventExists # ev)
    )
  $ runDb $ insertEventQuery ev

insertEventQuery :: Ev.Event
                 -> DB context err (EventInfo, Schema.EventId)
insertEventQuery
  event@(Ev.Event _eType _foreignEventId dwhat dwhen dwhy dwhere) = do
  -- insertEvent has to be the first thing that happens here so that
  -- uniqueness of the JSON event is enforced
  (evInfo, eventId) <- insertEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  let labelWithTypes = getLabelsWithType dwhat
      labelEpcs = getLabel <$> labelWithTypes
      labelTypes = getLabelType <$> labelWithTypes
  labelIds' <- mapM insertLabel labelEpcs
  let labelIds = Schema.LabelId <$> labelIds'
      labelIdWithTypes = zip labelTypes labelIds
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  mapM_ (\(mLblType, lblId) -> insertWhatLabel mLblType (Schema.WhatId whatId) lblId) labelIdWithTypes
  mapM_ (\(mLblType, lblId) -> insertLabelEvent mLblType eventId lblId) labelIdWithTypes
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
