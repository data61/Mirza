{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Mirza.SupplyChain.EventUtils
  ( insertEvent
  , insertDWhat, insertDWhen, insertDWhere, insertDWhy
  , insertWhatLabel, insertLabelEvent, insertLabel
  , hasUserCreatedEvent
  , insertUserEvent
  , findEvent, findSchemaEvent
  , findLabelId
  , getEventList
  , getUser, getUserById
  , findDWhere
  ) where

import qualified Mirza.Common.GS1BeamOrphans       as MU
import           Mirza.Common.Time                 (toDbTimestamp)
import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.ErrorUtils      (throwBackendError)
import           Mirza.SupplyChain.Types           hiding (User (..))
import qualified Mirza.SupplyChain.Types           as ST

import qualified Mirza.SupplyChain.QueryUtils      as QU


import           Data.GS1.EPC                      as EPC
import           Data.GS1.Event                    (Event (..))
import qualified Data.GS1.Event                    as Ev
import qualified Data.GS1.EventId                  as EvId

import           Data.GS1.DWhat                    (AggregationDWhat (..),
                                                    DWhat (..), LabelEPC (..),
                                                    ObjectDWhat (..),
                                                    ParentLabel (..),
                                                    TransactionDWhat (..),
                                                    TransformationDWhat (..))
import           Data.GS1.DWhen                    (DWhen (..))
import           Data.GS1.DWhere                   (BizLocation (..),
                                                    DWhere (..),
                                                    ReadPointLocation (..),
                                                    SrcDestLocation (..))
import           Data.GS1.DWhy                     (DWhy (..))

import           Data.Maybe                        (catMaybes)

import           Data.ByteString                   (ByteString)
import qualified Data.Text                         as T

import           Data.Time.LocalTime               (timeZoneOffsetString)

import           Database.Beam                     as B
import           Database.Beam.Postgres            (PgJSON (..))

import           Control.Monad                     (void)

import           Crypto.JOSE.Types                 (Base64Octets (..))

-- Helper functions

epcToStorageLabel :: Maybe MU.LabelType
                  -> Schema.WhatId
                  -> Schema.LabelId
                  -> LabelEPC
                  -> Schema.Label
epcToStorageLabel labelType (Schema.WhatId whatId) (Schema.LabelId pKey) (IL (SGTIN gs1Prefix fv ir sn)) =
  Schema.Label Nothing pKey labelType (Schema.WhatId whatId)
           gs1Prefix (Just ir)
           (Just sn) Nothing Nothing
           fv
           Nothing Nothing Nothing

epcToStorageLabel labelType (Schema.WhatId whatId) (Schema.LabelId pKey) (IL (GIAI gs1Prefix sn)) =
  Schema.Label Nothing pKey labelType (Schema.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing Nothing Nothing Nothing

epcToStorageLabel labelType (Schema.WhatId whatId) (Schema.LabelId pKey) (IL (SSCC gs1Prefix sn)) =
  Schema.Label Nothing pKey labelType (Schema.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing Nothing Nothing Nothing

epcToStorageLabel labelType (Schema.WhatId whatId) (Schema.LabelId pKey) (IL (GRAI gs1Prefix at sn)) =
  Schema.Label Nothing pKey labelType (Schema.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing (Just at) Nothing Nothing

epcToStorageLabel labelType (Schema.WhatId whatId) (Schema.LabelId pKey) (CL (LGTIN gs1Prefix ir lot) mQ) =
  Schema.Label Nothing pKey labelType (Schema.WhatId whatId)
           gs1Prefix (Just ir) Nothing
           Nothing (Just lot) Nothing Nothing
           (getQuantityAmount mQ) (getQuantityUom mQ)

epcToStorageLabel labelType (Schema.WhatId whatId) (Schema.LabelId pKey) (CL (CSGTIN gs1Prefix fv ir) mQ) =
  Schema.Label Nothing pKey labelType (Schema.WhatId whatId)
           gs1Prefix (Just ir) Nothing
           Nothing Nothing fv Nothing
           (getQuantityAmount mQ) (getQuantityUom mQ)

getQuantityAmount :: Maybe Quantity -> Maybe Amount
getQuantityAmount Nothing                       = Nothing
getQuantityAmount (Just (MeasuredQuantity a _)) = Just . Amount . realToFrac . unAmount $ a
getQuantityAmount (Just (ItemCount c))          = Just . Amount . realToFrac $ c

getQuantityUom :: Maybe Quantity -> Maybe EPC.Uom
getQuantityUom Nothing                       = Nothing
getQuantityUom (Just (MeasuredQuantity _ u)) = Just u
getQuantityUom (Just (ItemCount _))          = Nothing


-- | GS1 DWhat to Storage DWhat
-- For an object event
toStorageDWhat :: PrimaryKeyType
               -> Maybe PrimaryKeyType
               -> Maybe PrimaryKeyType
               -> Schema.EventId
               -> DWhat
               -> Schema.What
toStorageDWhat pKey mParentId mBizTranId eventId dwhat
   = Schema.What Nothing pKey
        (Just . Ev.getEventType $ dwhat)
        (getAction dwhat)
        (Schema.LabelId mParentId)
        (Schema.BizTransactionId mBizTranId)
        (Schema.TransformationId $ unTransformationId <$> (getTransformationId dwhat))
        eventId

getTransformationId :: DWhat -> Maybe EPC.TransformationId
getTransformationId (TransformWhat t) = _transformationId t
getTransformationId _                 = Nothing


getAction :: DWhat -> Maybe Action
getAction (TransformWhat _)                           = Nothing
getAction (ObjWhat (ObjectDWhat act _))               = Just act
getAction (TransactWhat (TransactionDWhat act _ _ _)) = Just act
getAction (AggWhat (AggregationDWhat act _ _))        = Just act


findInstLabelId :: InstanceLabelEPC -> DB context err [PrimaryKeyType]
findInstLabelId (GIAI cp sn) = findInstLabelId' cp sn Nothing Nothing Nothing
findInstLabelId (SSCC cp sn) = findInstLabelId' cp sn Nothing Nothing Nothing
findInstLabelId (SGTIN cp msfv ir sn) = findInstLabelId' cp sn msfv (Just ir) Nothing
findInstLabelId (GRAI cp at sn) = findInstLabelId' cp sn Nothing Nothing (Just at)


findInstLabelId' :: GS1CompanyPrefix
                 -> SerialNumber
                 -> Maybe SGTINFilterValue
                 -> Maybe ItemReference
                 -> Maybe AssetType
                 -> DB context err [PrimaryKeyType]
findInstLabelId' cp sn msfv mir mat = do
  l <- pg $ runSelectReturningList $ select $ do
    labels <- all_ (Schema._labels Schema.supplyChainDb)
    guard_ (Schema.label_gs1_company_prefix labels ==. val_ cp &&.
            Schema.label_serial_number labels ==. val_ (Just sn) &&.
            (Schema.label_sgtin_filter_value labels) ==. (val_ msfv) &&.
            Schema.label_asset_type labels ==. val_ mat &&.
            Schema.label_item_reference labels ==. val_ mir)
    pure labels
  pure $ Schema.label_id <$> l


getUser :: EmailAddress -> DB context err (Maybe ST.User)
getUser userEmail = do
  r <- pg $ runSelectReturningList $ select $ do
    allUsers <- all_ (Schema._users Schema.supplyChainDb)
    guard_ (Schema.user_email_address allUsers ==. val_ userEmail)
    pure allUsers
  pure $ case r of
    [u] -> Just . QU.userTableToModel $ u
    _   -> Nothing

findClassLabelId :: ClassLabelEPC -> DB context err [PrimaryKeyType]
findClassLabelId (LGTIN cp ir lot)  = findClassLabelId' cp Nothing ir (Just lot)
findClassLabelId (CSGTIN cp msfv ir) = findClassLabelId' cp msfv ir Nothing

findClassLabelId' :: GS1CompanyPrefix
                  -> Maybe SGTINFilterValue
                  -> ItemReference
                  -> Maybe Lot
                  -> DB context err [PrimaryKeyType]
findClassLabelId' cp msfv ir lot = do
  l <- pg $ runSelectReturningList $ select $ do
    labels <- all_ (Schema._labels Schema.supplyChainDb)
    guard_ (
             Schema.label_gs1_company_prefix labels ==. val_ cp &&.
             (Schema.label_sgtin_filter_value labels) ==. (val_ msfv) &&.
             Schema.label_lot labels ==. (val_ lot) &&.
             Schema.label_item_reference labels ==. (val_ . Just $ ir)
           )
    pure labels
  pure $ Schema.label_id <$> l


findLabelId :: LabelEPC -> DB context err [PrimaryKeyType]
findLabelId (IL l)   = findInstLabelId l
findLabelId (CL c _) = findClassLabelId c

getParentId :: DWhat -> DB context err (Maybe PrimaryKeyType)
getParentId (TransactWhat (TransactionDWhat _ (Just p) _ _)) = do
  res <- findInstLabelId . unParentLabel $ p
  pure $ case res of
    [l] -> Just l
    []  -> Nothing
    -- this should never happen.
    -- A call to error is here until a new error type is made
    _   -> error "There only should be 0/1 parent labels"
getParentId (AggWhat (AggregationDWhat _ (Just p) _) ) = do
  res <- findInstLabelId . unParentLabel $ p
  pure $ case res of
    [l] -> Just l
    []  -> Nothing
    -- this should never happen.
    -- A call to error is here until a new error type is made
    _   -> error "There only should be 0/1 parent labels"
getParentId _  = pure Nothing

toStorageDWhen :: Schema.WhenId
               -> DWhen
               -> Schema.EventId
               -> Schema.When
toStorageDWhen (Schema.WhenId pKey) (DWhen eventTime mRecordTime tZone) =
  Schema.When Nothing pKey
    (toDbTimestamp eventTime)
    (toDbTimestamp <$> mRecordTime)
    (T.pack . timeZoneOffsetString $ tZone)


toStorageDWhy :: Schema.WhyId -> DWhy -> Schema.EventId -> Schema.Why
toStorageDWhy (Schema.WhyId pKey) (DWhy mBiz mDisp)
  = Schema.Why Nothing pKey (renderURL <$> mBiz) (renderURL <$> mDisp)

toStorageEvent :: Schema.EventId
               -> Maybe EvId.EventId
               -> Schema.UserId
               -> PgJSON Ev.Event
               -> ByteString
               -> Schema.Event
toStorageEvent (Schema.EventId pKey) mEventId =
  Schema.Event Nothing pKey (EvId.unEventId <$> mEventId)

insertDWhat :: Maybe PrimaryKeyType
            -> DWhat
            -> Schema.EventId
            -> DB context err PrimaryKeyType
insertDWhat mBizTranId dwhat eventId = QU.withPKey $ \pKey ->  do
    mParentId <- getParentId dwhat
    pg $ B.runInsert $ B.insert (Schema._whats Schema.supplyChainDb)
          $ insertValues [toStorageDWhat pKey mParentId mBizTranId eventId dwhat]


insertDWhen :: DWhen -> Schema.EventId -> DB context err PrimaryKeyType
insertDWhen dwhen eventId = QU.withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (Schema._whens Schema.supplyChainDb)
             $ insertValues [toStorageDWhen (Schema.WhenId pKey) dwhen eventId]


insertDWhy :: DWhy -> Schema.EventId -> DB context err PrimaryKeyType
insertDWhy dwhy eventId = QU.withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (Schema._whys Schema.supplyChainDb)
             $ insertValues [toStorageDWhy (Schema.WhyId pKey) dwhy eventId]

insertSrcDestType :: MU.LocationField
                  -> Schema.EventId
                  -> SrcDestLocation
                  -> DB context err PrimaryKeyType
insertSrcDestType locField eventId
  (SrcDestLocation (sdType, SGLN pfix locationRef ext)) =
  QU.withPKey $ \pKey -> do
    let stWhere = Schema.Where Nothing pKey pfix (Just sdType) locationRef locField ext eventId
    pg $ B.runInsert $ B.insert (Schema._wheres Schema.supplyChainDb)
             $ insertValues [stWhere]

insertLocationEPC :: MU.LocationField
                  -> Schema.EventId
                  -> LocationEPC
                  -> DB context err PrimaryKeyType
insertLocationEPC locField eventId (SGLN pfix locationRef ext) =
  QU.withPKey $ \pKey -> do
    let stWhere = Schema.Where Nothing pKey pfix Nothing locationRef locField ext eventId
    pg $ B.runInsert $ B.insert (Schema._wheres Schema.supplyChainDb)
                $ insertValues [stWhere]

-- | Maps the relevant insert function for all
-- ReadPoint, BizLocation, Src, Dest
insertDWhere :: DWhere -> Schema.EventId -> DB context err ()
insertDWhere (DWhere rPoints bizLocs srcTs destTs) eventId = do
    sequence_ $ insertLocationEPC MU.ReadPoint eventId . unReadPointLocation <$> rPoints
    sequence_ $ insertLocationEPC MU.BizLocation eventId . unBizLocation <$> bizLocs
    sequence_ $ insertSrcDestType MU.Src eventId <$> srcTs
    sequence_ $ insertSrcDestType MU.Dest eventId <$> destTs

-- | Given a DWhere, looks for all the insertions associated with the DWHere
-- Think of this as the inverse of ``insertDWhere``
findDWhere :: Schema.EventId -> DB context err (Maybe DWhere)
findDWhere eventId = do
  rPoints <- findDWhereByLocationField MU.ReadPoint eventId
  bizLocs <- findDWhereByLocationField MU.BizLocation eventId
  srcTs <- findDWhereByLocationField MU.Src eventId
  destTs <- findDWhereByLocationField MU.Dest eventId
  pure $ mergeSBWheres [rPoints, bizLocs, srcTs, destTs]

findDWhereByLocationField :: MU.LocationField -> Schema.EventId -> DB context err [Schema.WhereT Identity]
findDWhereByLocationField locField eventId = pg $ runSelectReturningList $ select $ do
    wheres <- all_ (Schema._wheres Schema.supplyChainDb)
    guard_ (
      Schema.where_event_id wheres ==. val_ eventId &&.
      Schema.where_location_field wheres ==. val_ locField)
    pure wheres

-- | Merges a list of Schema.Wheres into one Data.GS1.DWhere
-- mergeSBWheres :: [Schema.WhereT Identity] -> DWhere
mergeSBWheres :: [[Schema.WhereT Identity]] -> Maybe DWhere
mergeSBWheres [rPointsW, bizLocsW, srcTsW, destTsW] =
  let rPoints = ReadPointLocation . constructLocation <$> rPointsW
      bizLocs = BizLocation . constructLocation <$> bizLocsW
      srcTs = constructSrcDestLocation <$> srcTsW
      destTs = constructSrcDestLocation <$> destTsW
      in
        DWhere rPoints bizLocs <$> sequence srcTs <*> sequence destTs
mergeSBWheres _                                     = Nothing -- error "Invalid arguments"

-- | This relies on the user calling this function in the appropriate WhereT
constructSrcDestLocation :: Schema.WhereT Identity -> Maybe SrcDestLocation
constructSrcDestLocation whereT =
  SrcDestLocation . (,constructLocation whereT)
    <$> Schema.where_source_dest_type whereT


-- | This relies on the user calling this function in the appropriate WhereT
constructLocation :: Schema.WhereT Identity -> LocationEPC
constructLocation whereT =
  EPC.SGLN
    (Schema.where_gs1_company_prefix whereT)
    (Schema.where_gs1_location_id whereT)
    (Schema.where_sgln_ext whereT)


insertEvent :: Schema.UserId
            -> Ev.Event
            -> DB context err (EventInfo, Schema.EventId)
insertEvent userId@(Schema.UserId uuid) event = do
  let toSignEvent = QU.constructEventToSign event
  eventId <- fmap (Schema.EventId <$>) QU.withPKey $ \pKey ->
    pg $ B.runInsert $ B.insert (Schema._events Schema.supplyChainDb)
        $ insertValues
            [toStorageEvent (Schema.EventId pKey) (_eid event)
              userId (PgJSON event) toSignEvent]
  pure ((EventInfo event [] [(ST.UserId uuid)] (Base64Octets toSignEvent) NeedMoreSignatures),
      eventId)


insertUserEvent :: Schema.EventId
                -> EventOwner
                -> Bool
                -> (Maybe ByteString)
                -> SigningUser
                -> DB context err ()
insertUserEvent eventId (EventOwner addedByUserId) signed signedHash (SigningUser userId) =
  let signingId = Schema.UserId . getUserId $ userId
      ownerId = Schema.UserId . getUserId $ addedByUserId
  in
    void $ QU.withPKey $ \pKey ->
      pg $ B.runInsert $ B.insert (Schema._user_events Schema.supplyChainDb)
          $ insertValues
            [ Schema.UserEvent Nothing  pKey eventId signingId signed ownerId signedHash
            ]

insertWhatLabel :: Schema.WhatId
                -> Schema.LabelId
                -> DB context err PrimaryKeyType
insertWhatLabel (Schema.WhatId whatId) (Schema.LabelId labelId) = QU.withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (Schema._what_labels Schema.supplyChainDb)
        $ insertValues
        [
          Schema.WhatLabel Nothing pKey
          (Schema.WhatId whatId)
          (Schema.LabelId labelId)

        ]

-- | Given the necessary information,
-- converts a ``LabelEPC`` to Schema.Label and writes it to the database
insertLabel :: Maybe MU.LabelType
            -> Schema.WhatId
            -> LabelEPC
            -> DB context err PrimaryKeyType
insertLabel labelType whatId labelEpc = QU.withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (Schema._labels Schema.supplyChainDb)
        $ insertValues
        [ epcToStorageLabel labelType whatId (Schema.LabelId pKey) labelEpc]

-- | Ties up a label and an event entry in the database
insertLabelEvent :: Schema.EventId
                 -> Schema.LabelId
                 -> DB context err PrimaryKeyType
insertLabelEvent (Schema.EventId eventId) (Schema.LabelId labelId) = QU.withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (Schema._label_events Schema.supplyChainDb)
        $ insertValues
          [ Schema.LabelEvent Nothing pKey (Schema.LabelId labelId)
              (Schema.EventId eventId)
        ]

getUserById :: ST.UserId -> DB context err (Maybe Schema.User)
getUserById (ST.UserId uid) = do
  r <- pg $ runSelectReturningList $ select $ do
          user <- all_ (Schema._users Schema.supplyChainDb)
          guard_ (Schema.user_id user ==. val_ uid)
          pure user
  case r of
    [user] -> pure $ Just user
    _      -> pure Nothing

getEventList :: AsServiceError err
             => Schema.LabelId
             -> DB context err [Ev.Event]
getEventList (Schema.LabelId labelId) = do
  labelEvents <- pg $ runSelectReturningList $ select $ do
        labelEvent <- all_ (Schema._label_events Schema.supplyChainDb)
        guard_ (Schema.label_event_label_id labelEvent ==. val_ (Schema.LabelId labelId))
        pure labelEvent
  let eventIds = Schema.label_event_event_id <$> labelEvents
      allEvents = findEvent <$> eventIds
  catMaybes <$> sequence allEvents

findEvent :: AsServiceError err
          => Schema.EventId
          -> DB context err (Maybe Ev.Event)
findEvent eventId = do
  mschemaEvent <- findSchemaEvent eventId
  pure $ mschemaEvent >>= (Just . QU.storageToModelEvent)

findSchemaEvent :: AsServiceError err
                => Schema.EventId
                -> DB context err (Maybe Schema.Event)
findSchemaEvent (Schema.EventId eventId) = do
  r <- pg $
        runSelectReturningList $ select $ do
        event <- all_ (Schema._events Schema.supplyChainDb)
        guard_ (Schema.event_id event ==. (val_ eventId))
        pure event
  case r of
    [event] -> pure $ Just event
    []      -> pure Nothing
    -- TODO: Do the right thing here
    _       -> throwBackendError r

-- | Checks if a user is associated with an event
hasUserCreatedEvent :: ST.UserId -> EvId.EventId -> DB context err Bool
hasUserCreatedEvent (ST.UserId userId) (EvId.EventId eventId) = do
  r <- pg $ runSelectReturningList $ select $ do
        userEvent <- all_ (Schema._user_events Schema.supplyChainDb)
        guard_ (Schema.user_events_owner userEvent ==. (val_ . Schema.UserId $ userId) &&.
                Schema.user_events_event_id userEvent ==. (val_ . Schema.EventId $ eventId))
        pure userEvent
  pure $ case r of
    [_userEvent] -> True
    _            -> False

