{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

-- | Following are a bunch of utility functions to do household stuff like
-- generating primary keys, timestamps - stuff that almost every function
-- below would need to do anyway
-- functions that start with `insert` does some database operation
-- functions that start with `to` converts between
-- Model type and its Storage equivalent
module Mirza.SupplyChain.QueryUtils where

import           Mirza.SupplyChain.AppConfig    (AppErr, DB, SCSContext, pg)
import           Mirza.SupplyChain.ErrorUtils   (throwBackendError)
import qualified Mirza.SupplyChain.MigrateUtils as MU
import qualified Mirza.SupplyChain.Model        as M
import qualified Mirza.SupplyChain.StorageBeam  as SB

import           Data.GS1.DWhy                  (DWhy (..))
import           Data.GS1.EPC                   as EPC
import           Data.GS1.Event                 (Event (..))
import qualified Data.GS1.Event                 as Ev
import qualified Data.GS1.EventID               as EvId

import           Control.Monad.IO.Class         (liftIO)
import           Data.Aeson                     (decode)
import           Data.Aeson.Text                (encodeToLazyText)
import           Data.ByteString                (ByteString)
import           Data.GS1.DWhat                 (AggregationDWhat (..),
                                                 DWhat (..), LabelEPC (..),
                                                 ObjectDWhat (..),
                                                 ParentLabel (..),
                                                 TransactionDWhat (..),
                                                 TransformationDWhat (..))
import           Data.GS1.DWhen                 (DWhen (..))
import           Data.GS1.DWhere                (BizLocation (..), DWhere (..),
                                                 ReadPointLocation (..),
                                                 SrcDestLocation (..))
import           Data.Maybe                     (catMaybes)
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as En
import qualified Data.Text.Lazy                 as TxtL
import qualified Data.Text.Lazy.Encoding        as LEn
import           Data.Time                      (UTCTime, ZonedTime (..),
                                                 utcToZonedTime)
import           Data.Time.Clock                (getCurrentTime)
import           Data.Time.LocalTime            (LocalTime, localTimeToUTC,
                                                 timeZoneOffsetString, utc,
                                                 utcToLocalTime)
import           Data.UUID.V4                   (nextRandom)
import           Database.Beam                  as B

import           Control.Monad                  (void)
import           Control.Monad.Except           (MonadError, catchError)

-- | Reads back the ``LocalTime`` in UTCTime (with an offset of 0)
toEPCISTime :: LocalTime -> EPCISTime
toEPCISTime t = EPCISTime (localTimeToUTC utc t)

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
toLocalTime :: EPCISTime -> LocalTime
toLocalTime = utcToLocalTime utc . unEPCISTime

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
toZonedTime :: UTCTime -> ZonedTime
toZonedTime = utcToZonedTime utc

-- | Generates a timestamp in LocalTime + 0:00 offset
-- which is a UTCTime
generateTimeStamp :: MonadIO m => m LocalTime
generateTimeStamp = utcToLocalTime utc <$> liftIO getCurrentTime


-- | shorthand for wrapping ``UUID.nextRandom`` in ``AppM``
generatePk :: MonadIO m => m SB.PrimaryKeyType
generatePk = liftIO nextRandom

-- | Converts a DB representation of ``User`` to a Model representation
-- SB.User = SB.User uid bizId fName lName phNum passHash email
userTableToModel :: SB.User -> M.User
userTableToModel (SB.User uid _ fName lName _ _ _) = M.User (M.UserID uid) fName lName

-- | Json encode the event
-- currently do it automagically, but might what to be
-- more systematic about it so it's easier to replicated. Maybe.
encodeEvent :: Event -> T.Text
encodeEvent event = TxtL.toStrict  (encodeToLazyText event)

-- XXX is this the right encoding to use? It's used for checking signatures
-- and hashing the json.
eventTxtToBS :: T.Text -> ByteString
eventTxtToBS = En.encodeUtf8

decodeEvent :: T.Text -> Maybe Ev.Event
decodeEvent = decode . LEn.encodeUtf8 . TxtL.fromStrict

epcToStorageLabel :: Maybe MU.LabelType
                  -> SB.WhatId
                  -> SB.PrimaryKeyType
                  -> LabelEPC
                  -> SB.Label
epcToStorageLabel labelType (SB.WhatId whatId) pKey (IL (SGTIN gs1Prefix fv ir sn)) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix (Just ir)
           (Just sn) Nothing Nothing
           fv
           Nothing Nothing Nothing

epcToStorageLabel labelType (SB.WhatId whatId) pKey (IL (GIAI gs1Prefix sn)) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing Nothing Nothing Nothing

epcToStorageLabel labelType (SB.WhatId whatId) pKey (IL (SSCC gs1Prefix sn)) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing Nothing Nothing Nothing

epcToStorageLabel labelType (SB.WhatId whatId) pKey (IL (GRAI gs1Prefix at sn)) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing (Just at) Nothing Nothing

epcToStorageLabel labelType (SB.WhatId whatId) pKey (CL (LGTIN gs1Prefix ir lot) mQ) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix (Just ir) Nothing
           Nothing (Just lot) Nothing Nothing
           (getQuantityAmount mQ) (getQuantityUom mQ)

epcToStorageLabel labelType (SB.WhatId whatId) pKey (CL (CSGTIN gs1Prefix fv ir) mQ) =
  SB.Label pKey labelType (SB.WhatId whatId)
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
toStorageDWhat :: SB.PrimaryKeyType
               -> Maybe SB.PrimaryKeyType
               -> Maybe SB.PrimaryKeyType
               -> SB.PrimaryKeyType
               -> DWhat
               -> SB.What
toStorageDWhat pKey mParentId mBizTranId eventId dwhat
   = SB.What pKey
        (Just . Ev.getEventType $ dwhat)
        (getAction dwhat)
        (SB.LabelId mParentId)
        (SB.BizTransactionId mBizTranId)
        (SB.TransformationId $ unTransformationID <$> (getTransformationId dwhat))
        (SB.EventId eventId)

getTransformationId :: DWhat -> Maybe TransformationID
getTransformationId (TransformWhat t) = _transformationId t
getTransformationId _                 = Nothing


getAction :: DWhat -> Maybe Action
getAction (TransformWhat _)                           = Nothing
getAction (ObjWhat (ObjectDWhat act _))               = Just act
getAction (TransactWhat (TransactionDWhat act _ _ _)) = Just act
getAction (AggWhat (AggregationDWhat act _ _))        = Just act


findInstLabelId :: InstanceLabelEPC -> DB SCSContext AppErr (Maybe SB.PrimaryKeyType)
findInstLabelId (GIAI cp sn) = findInstLabelId' cp sn Nothing Nothing Nothing
findInstLabelId (SSCC cp sn) = findInstLabelId' cp sn Nothing Nothing Nothing
findInstLabelId (SGTIN cp msfv ir sn) = findInstLabelId' cp sn msfv (Just ir) Nothing
findInstLabelId (GRAI cp at sn) = findInstLabelId' cp sn Nothing Nothing (Just at)

-- | Ueful for handling specific errors from, for example, database transactions
-- @
--  handleError errHandler $ runDb ...
--  ...
--  where errHandler (AppErr (DatabaseError sqlErr)) = ...
--        errHandler e = throwError e
-- @
handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

-- | Handles the common case of generating a primary key, using it in some
-- transaction and then returning the primary key.
-- @
--   insertFoo :: ArgType -> AppM PrimmaryKeyType
--   insertFoo arg = withPKey $ \pKey -> do
--     runDb ... pKey ...
-- @
withPKey :: MonadIO m => (SB.PrimaryKeyType -> m a) -> m SB.PrimaryKeyType
withPKey f = do
  pKey <- generatePk
  _ <- f pKey
  pure pKey

findInstLabelId' :: GS1CompanyPrefix
                -> SerialNumber
                -> Maybe SGTINFilterValue
                -> Maybe ItemReference
                -> Maybe AssetType
                -> DB SCSContext AppErr (Maybe SB.PrimaryKeyType)
findInstLabelId' cp sn msfv mir mat = do
  r <- pg $ runSelectReturningList $ select $ do
    labels <- all_ (SB._labels SB.supplyChainDb)
    guard_ (SB.label_gs1_company_prefix labels ==. val_ cp &&.
            SB.serial_number labels ==. val_ (Just sn) &&.
            (SB.sgtin_filter_value labels) ==. (val_ msfv) &&.
            SB.asset_type labels ==. val_ mat &&.
            SB.item_reference labels ==. val_ mir)
    pure labels
  return $ case r of
    [l] -> Just (SB.label_id l)
    _   -> Nothing


getUser :: M.EmailAddress -> DB SCSContext AppErr (Maybe M.User)
getUser (M.EmailAddress email) = do
  r <- pg $ runSelectReturningList $ select $ do
    allUsers <- all_ (SB._users SB.supplyChainDb)
    guard_ (SB.email_address allUsers ==. val_ email)
    pure allUsers
  return $ case r of
    [u] -> Just . userTableToModel $ u
    _   -> Nothing

findClassLabelId :: ClassLabelEPC -> DB SCSContext AppErr (Maybe SB.PrimaryKeyType)
findClassLabelId (LGTIN cp ir lot)  = findClassLabelId' cp Nothing ir (Just lot)
findClassLabelId (CSGTIN cp msfv ir) = findClassLabelId' cp msfv ir Nothing

findClassLabelId' :: GS1CompanyPrefix
                 -> Maybe SGTINFilterValue
                 -> ItemReference
                 -> Maybe Lot
                 -> DB SCSContext AppErr (Maybe SB.PrimaryKeyType)
findClassLabelId' cp msfv ir lot = do
  r <- pg $ runSelectReturningList $ select $ do
    labels <- all_ (SB._labels SB.supplyChainDb)
    guard_ (
             SB.label_gs1_company_prefix labels ==. val_ cp &&.
             (SB.sgtin_filter_value labels) ==. (val_ msfv) &&.
             SB.lot labels ==. (val_ lot) &&.
             SB.item_reference labels ==. (val_ . Just $ ir)
           )
    pure labels
  case r of
    [l] -> return $ Just (SB.label_id l)
    _   -> return Nothing


findLabelId :: LabelEPC -> DB SCSContext AppErr (Maybe SB.PrimaryKeyType)
findLabelId (IL l)   = findInstLabelId l
findLabelId (CL c _) = findClassLabelId c

getParentId :: DWhat -> DB SCSContext AppErr (Maybe SB.PrimaryKeyType)
getParentId (TransactWhat (TransactionDWhat _ (Just p) _ _)) = findInstLabelId . unParentLabel $ p
getParentId (AggWhat (AggregationDWhat _ (Just p) _) )  = findInstLabelId . unParentLabel $ p
getParentId _                                 = return Nothing

toStorageDWhen :: SB.PrimaryKeyType
               -> DWhen
               -> SB.PrimaryKeyType
               -> SB.When
toStorageDWhen pKey (DWhen eventTime mRecordTime tZone) eventId =
  SB.When pKey
    (toLocalTime eventTime)
    (toLocalTime <$> mRecordTime)
    (T.pack . timeZoneOffsetString $ tZone)
    (SB.EventId eventId)

toStorageDWhy :: SB.PrimaryKeyType -> DWhy -> SB.PrimaryKeyType -> SB.Why
toStorageDWhy pKey (DWhy mBiz mDisp) eventId =
  SB.Why pKey
    (renderURL <$> mBiz)
    (renderURL <$> mDisp)
    (SB.EventId eventId)

toStorageEvent :: SB.PrimaryKeyType
               -> SB.PrimaryKeyType
               -> T.Text
               -> Maybe EvId.EventID
               -> SB.Event
toStorageEvent pKey userId jsonEvent mEventId =
  SB.Event pKey (EvId.getEventId <$> mEventId) (SB.UserId userId) jsonEvent

insertDWhat :: Maybe SB.PrimaryKeyType
            -> DWhat
            -> SB.PrimaryKeyType
            -> DB SCSContext AppErr SB.PrimaryKeyType
insertDWhat mBizTranId dwhat eventId = withPKey $ \pKey ->  do
    mParentId <- getParentId dwhat
    pg $ B.runInsert $ B.insert (SB._whats SB.supplyChainDb)
          $ insertValues [toStorageDWhat pKey mParentId mBizTranId eventId dwhat]


insertDWhen :: DWhen -> SB.PrimaryKeyType -> DB SCSContext AppErr SB.PrimaryKeyType
insertDWhen dwhen eventId = withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (SB._whens SB.supplyChainDb)
             $ insertValues [toStorageDWhen pKey dwhen eventId]


insertDWhy :: DWhy -> SB.PrimaryKeyType -> DB SCSContext AppErr SB.PrimaryKeyType
insertDWhy dwhy eventId = withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (SB._whys SB.supplyChainDb)
             $ insertValues [toStorageDWhy pKey dwhy eventId]

insertSrcDestType :: MU.LocationField
                  -> SB.PrimaryKeyType
                  -> SrcDestLocation
                  -> DB SCSContext AppErr SB.PrimaryKeyType
insertSrcDestType locField eventId
  (SrcDestLocation (sdType, SGLN pfix locationRef ext)) =
  withPKey $ \pKey -> do
    let stWhere = SB.Where pKey pfix (Just sdType) locationRef locField ext
                (SB.EventId eventId)
    pg $ B.runInsert $ B.insert (SB._wheres SB.supplyChainDb)
             $ insertValues [stWhere]

insertLocationEPC :: MU.LocationField
                  -> SB.PrimaryKeyType
                  -> LocationEPC
                  -> DB SCSContext AppErr SB.PrimaryKeyType
insertLocationEPC locField eventId (SGLN pfix locationRef ext) =
  withPKey $ \pKey -> do
    let stWhere = SB.Where pKey pfix Nothing locationRef locField ext
                  (SB.EventId eventId)
    pg $ B.runInsert $ B.insert (SB._wheres SB.supplyChainDb)
                $ insertValues [stWhere]

-- | Maps the relevant insert function for all
-- ReadPoint, BizLocation, Src, Dest
insertDWhere :: DWhere -> SB.PrimaryKeyType -> DB SCSContext AppErr ()
insertDWhere (DWhere rPoints bizLocs srcTs destTs) eventId = do
    sequence_ $ insertLocationEPC MU.ReadPoint eventId . unReadPointLocation <$> rPoints
    sequence_ $ insertLocationEPC MU.BizLocation eventId . unBizLocation <$> bizLocs
    sequence_ $ insertSrcDestType MU.Src eventId <$> srcTs
    sequence_ $ insertSrcDestType MU.Dest eventId <$> destTs

-- | Given a DWhere, looks for all the insertions associated with the DWHere
-- Think of this as the inverse of ``insertDWhere``
findDWhere :: SB.PrimaryKeyType -> DB SCSContext AppErr (Maybe DWhere)
findDWhere eventId = do
  rPoints <- findDWhereByLocationField MU.ReadPoint eventId
  bizLocs <- findDWhereByLocationField MU.BizLocation eventId
  srcTs <- findDWhereByLocationField MU.Src eventId
  destTs <- findDWhereByLocationField MU.Dest eventId
  return $ mergeSBWheres [rPoints, bizLocs, srcTs, destTs]

findDWhereByLocationField :: MU.LocationField -> SB.PrimaryKeyType -> DB SCSContext AppErr [SB.WhereT Identity]
findDWhereByLocationField locField eventId = pg $ runSelectReturningList $ select $ do
    wheres <- all_ (SB._wheres SB.supplyChainDb)
    guard_ (
      SB.where_event_id wheres ==. val_ (SB.EventId eventId) &&.
      SB.where_location_field wheres ==. val_ locField)
    pure wheres

-- | Merges a list of SB.Wheres into one Data.GS1.DWhere
-- mergeSBWheres :: [SB.WhereT Identity] -> DWhere
mergeSBWheres :: [[SB.WhereT Identity]] -> Maybe DWhere
mergeSBWheres [rPointsW, bizLocsW, srcTsW, destTsW] =
  let rPoints = ReadPointLocation . constructLocation <$> rPointsW
      bizLocs = BizLocation . constructLocation <$> bizLocsW
      srcTs = constructSrcDestLocation <$> srcTsW
      destTs = constructSrcDestLocation <$> destTsW
      in
        DWhere rPoints bizLocs <$> sequence srcTs <*> sequence destTs
mergeSBWheres _                                     = Nothing -- error "Invalid arguments"

-- | This relies on the user calling this function in the appropriate WhereT
constructSrcDestLocation :: SB.WhereT Identity -> Maybe SrcDestLocation
constructSrcDestLocation whereT =
  SrcDestLocation . (,constructLocation whereT)
    <$> SB.where_source_dest_type whereT


-- | This relies on the user calling this function in the appropriate WhereT
constructLocation :: SB.WhereT Identity -> LocationEPC
constructLocation whereT =
  EPC.SGLN
    (SB.where_gs1_company_prefix whereT)
    (SB.where_gs1_location_id whereT)
    (SB.where_sgln_ext whereT)


insertEvent :: SB.PrimaryKeyType -> T.Text -> Event -> DB SCSContext AppErr SB.PrimaryKeyType
insertEvent userId jsonEvent event = withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (SB._events SB.supplyChainDb)
        $ insertValues [toStorageEvent pKey userId jsonEvent (_eid event)]

insertUserEvent :: SB.PrimaryKeyType
                -> SB.PrimaryKeyType
                -> SB.PrimaryKeyType
                -> Bool
                -> (Maybe ByteString)
                -> DB SCSContext AppErr ()
insertUserEvent eventId userId addedByUserId signed signedHash =
  void $ withPKey $ \pKey ->
    pg $ B.runInsert $ B.insert (SB._user_events SB.supplyChainDb)
        $ insertValues
          [ SB.UserEvent pKey (SB.EventId eventId) (SB.UserId userId)
                        signed (SB.UserId addedByUserId) signedHash
          ]

insertWhatLabel :: SB.WhatId
                -> SB.LabelId
                -> DB SCSContext AppErr SB.PrimaryKeyType
insertWhatLabel (SB.WhatId whatId) (SB.LabelId labelId) = withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (SB._what_labels SB.supplyChainDb)
        $ insertValues
        [
          SB.WhatLabel pKey
          (SB.WhatId whatId)
          (SB.LabelId labelId)
        ]

-- | Given the necessary information,
-- converts a ``LabelEPC`` to SB.Label and writes it to the database
insertLabel :: Maybe MU.LabelType
            -> SB.WhatId
            -> LabelEPC
            -> DB SCSContext AppErr SB.PrimaryKeyType
insertLabel labelType (SB.WhatId whatId) labelEpc = withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (SB._labels SB.supplyChainDb)
        $ insertValues
        [ epcToStorageLabel labelType (SB.WhatId whatId) pKey labelEpc]

-- | Ties up a label and an event entry in the database
insertLabelEvent :: SB.EventId
                 -> SB.LabelId
                 -> DB SCSContext AppErr SB.PrimaryKeyType
insertLabelEvent (SB.EventId eventId) (SB.LabelId labelId) = withPKey $ \pKey ->
  pg $ B.runInsert $ B.insert (SB._label_events SB.supplyChainDb)
        $ insertValues
          [ SB.LabelEvent pKey (SB.LabelId labelId) (SB.EventId eventId)
        ]


selectUser :: M.UserID -> DB SCSContext AppErr (Maybe SB.User)
selectUser (M.UserID uid) = do
  r <- pg $ runSelectReturningList $ select $ do
          user <- all_ (SB._users SB.supplyChainDb)
          guard_ (SB.user_id user ==. val_ uid)
          pure user
  case r of
    [user] -> return $ Just user
    _      -> return Nothing

getEventList :: SB.LabelId -> DB SCSContext AppErr [Ev.Event]
getEventList (SB.LabelId labelId) = do
  labelEvents <- pg $ runSelectReturningList $ select $ do
        labelEvent <- all_ (SB._label_events SB.supplyChainDb)
        guard_ (SB.label_event_label_id labelEvent ==. val_ (SB.LabelId labelId))
        pure labelEvent
  let eventIds = SB.label_event_event_id <$> labelEvents
      allEvents = findEvent <$> eventIds
  catMaybes <$> sequence allEvents

findEvent :: SB.EventId -> DB SCSContext AppErr (Maybe Ev.Event)
findEvent (SB.EventId eventId) = do
  r <- pg $
        runSelectReturningList $ select $ do
        event <- all_ (SB._events SB.supplyChainDb)
        guard_ (SB.event_id event ==. (val_ eventId))
        pure event
  case r of
    [event] -> return $ storageToModelEvent event
    -- TODO: Do the right thing here
    _       -> throwBackendError r

storageToModelEvent :: SB.Event -> Maybe Ev.Event
storageToModelEvent = decodeEvent . SB.json_event

-- | Checks if a pair of userIds are recorded as a contact.
-- __Must be run in a transaction!__
isExistingContact :: M.UserID -> M.UserID -> DB SCSContext AppErr Bool
isExistingContact (M.UserID uid1) (M.UserID uid2) = do
  r <- pg $ runSelectReturningList $ select $ do
        contact <- all_ (SB._contacts SB.supplyChainDb)
        guard_ (SB.contact_user1_id contact  ==. (val_ . SB.UserId $ uid1) &&.
                SB.contact_user2_id contact  ==. (val_ . SB.UserId $ uid2))
        pure contact
  return $ verifyContact r (SB.UserId uid1) (SB.UserId uid2)


-- | Simple utility function to check that the users are part of the contact
-- typically used with the result of a query
verifyContact :: Eq (PrimaryKey SB.UserT f) =>
                 [SB.ContactT f] ->
                 PrimaryKey SB.UserT f ->
                 PrimaryKey SB.UserT f ->
                 Bool
verifyContact [insertedContact] uid1 uid2 =
                  (SB.contact_user1_id insertedContact == uid1) &&
                  (SB.contact_user2_id insertedContact == uid2)
verifyContact _ _ _ = False

storageToModelBusiness :: SB.Business -> M.Business
storageToModelBusiness (SB.Business pfix name f site addr lat long)
  = M.Business pfix name f site addr lat long

