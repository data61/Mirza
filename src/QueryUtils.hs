{-# LANGUAGE OverloadedStrings #-}

-- | Following are a bunch of utility functions to do household stuff like
-- generating primary keys, timestamps - stuff that almost every function
-- below would need to do anyway
-- functions that start with `insert` does some database operation
-- functions that start with `to` converts between
-- Model type and its Storage equivalent
module QueryUtils where

import           Data.Time.LocalTime (utc, utcToLocalTime
                                     , LocalTime, localTimeToUTC
                                     , timeZoneOffsetString)
import           Data.Time (UTCTime, ZonedTime(..), utcToZonedTime
                           , zonedTimeToUTC)
import           AppConfig (AppM(..), runDb, getDBConn)
import qualified StorageBeam as SB
import           Data.UUID.V4 (nextRandom)
import           Data.Time.Clock (getCurrentTime)
import           Control.Monad.Reader (liftIO)
import           Data.Maybe (catMaybes)
import           Data.GS1.Event (Event(..))
import           Data.Aeson.Text (encodeToLazyText)
import           Data.Aeson (decode)
import qualified Data.Text.Lazy as TxtL
import qualified Data.Text as T
import           Data.GS1.EPC as EPC
import           Data.GS1.DWhat (DWhat(..), LabelEPC(..))
import           Data.GS1.DWhy (DWhy(..))
import           Data.GS1.DWhere (DWhere(..), SrcDestLocation)
import           Data.GS1.DWhen (DWhen(..))
import qualified Data.GS1.EventID as EvId
import qualified Data.GS1.Event as Ev
import           Utils
import           Database.Beam as B
import           Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import           Data.ByteString (ByteString)
import qualified Model as M
import           ErrorUtils (throwBackendError, throwAppError, toServerError
                            , defaultToServerError, sqlToServerError
                            , throwUnexpectedDBError)
import           Database.PostgreSQL.Simple
import           Data.Text.Lazy.Encoding (encodeUtf8)

-- | Reads back the ``LocalTime`` in UTCTime (with an offset of 0)
toEPCISTime :: LocalTime -> UTCTime
toEPCISTime = localTimeToUTC utc

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
toLocalTime :: UTCTime -> LocalTime
toLocalTime = utcToLocalTime utc

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
toZonedTime :: UTCTime -> ZonedTime
toZonedTime = utcToZonedTime utc

-- | Generates a timestamp in LocalTime + 0:00 offset
-- which is a UTCTime
generateTimeStamp :: AppM LocalTime
generateTimeStamp = do
  utcTime <- liftIO getCurrentTime
  return $ utcToLocalTime utc utcTime

-- | shorthand for wrapping ``UUID.nextRandom`` in ``AppM``
generatePk :: AppM SB.PrimaryKeyType
generatePk = liftIO $ nextRandom

-- | Converts a DB representation of ``User`` to a Model representation
-- SB.User = SB.User uid bizId fName lName phNum passHash email
userTableToModel :: SB.User -> M.User
userTableToModel (SB.User uid _ fName lName _ _ _) = M.User uid fName lName

-- | Json encode the event
-- currently do it automagically, but might what to be
-- more systematic about it so it's easier to replicated. Maybe.
encodeEvent :: Event -> T.Text
encodeEvent event = TxtL.toStrict  (encodeToLazyText event)

decodeEvent :: T.Text -> Maybe Ev.Event
decodeEvent jsonEvent = decode . encodeUtf8 . TxtL.fromStrict $ jsonEvent

epcToStorageLabel :: Maybe T.Text
                  -> SB.PrimaryKeyType
                  -> SB.PrimaryKeyType
                  -> LabelEPC
                  -> SB.Label
epcToStorageLabel labelType whatId pKey (IL (SGTIN gs1Prefix fv ir sn)) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix (Just ir)
           (Just sn) Nothing Nothing
           (toText <$> fv)
           Nothing Nothing Nothing

epcToStorageLabel labelType whatId pKey (IL (GIAI gs1Prefix sn)) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing Nothing Nothing Nothing

epcToStorageLabel labelType whatId pKey (IL (SSCC gs1Prefix sn)) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing Nothing Nothing Nothing

epcToStorageLabel labelType whatId pKey (IL (GRAI gs1Prefix at sn)) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix Nothing (Just sn)
           Nothing Nothing Nothing (Just at) Nothing Nothing

epcToStorageLabel labelType whatId pKey (CL (LGTIN gs1Prefix ir lot) mQ) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix (Just ir) Nothing
           Nothing (Just lot) Nothing Nothing
           (getQuantityAmount mQ) (getQuantityUom mQ)

epcToStorageLabel labelType whatId pKey (CL (CSGTIN gs1Prefix fv ir) mQ) =
  SB.Label pKey labelType (SB.WhatId whatId)
           gs1Prefix (Just ir) Nothing
           Nothing Nothing (toText <$> fv) Nothing
           (getQuantityAmount mQ) (getQuantityUom mQ)

getQuantityAmount :: Maybe Quantity -> Maybe Double
getQuantityAmount Nothing = Nothing
getQuantityAmount (Just (MeasuredQuantity a _)) = Just $ realToFrac a
getQuantityAmount (Just (ItemCount c)) = Just $ realToFrac c

getQuantityUom :: Maybe Quantity -> Maybe EPC.Uom
getQuantityUom Nothing = Nothing
getQuantityUom (Just (MeasuredQuantity _ u)) = Just u
getQuantityUom (Just (ItemCount _)) = Nothing


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
            (Just . Ev.stringify . Ev.getEventType $ dwhat)
            (toText <$> getAction dwhat)
            (SB.LabelId mParentId)
            (SB.BizTransactionId mBizTranId)
            (SB.TransformationId $ getTransformationId dwhat)
            (SB.EventId eventId)

getTransformationId :: DWhat -> Maybe TransformationID
getTransformationId t@(TransformationDWhat _ _ _) = _TransformationId t
getTransformationId _ = Nothing


getAction :: DWhat -> Maybe Action
getAction (TransformationDWhat _ _ _) = Nothing
getAction (ObjectDWhat act _) = Just act
getAction (TransactionDWhat act _ _ _) = Just act
getAction (AggregationDWhat act _ _) = Just act


findInstLabelId :: InstanceLabelEPC -> AppM (Maybe SB.PrimaryKeyType)
findInstLabelId (GIAI cp sn) = findInstLabelId' cp sn Nothing Nothing Nothing
findInstLabelId (SSCC cp sn) = findInstLabelId' cp sn Nothing Nothing Nothing
findInstLabelId (SGTIN cp msfv ir sn) = findInstLabelId' cp sn msfv (Just ir) Nothing
findInstLabelId (GRAI cp at sn) = findInstLabelId' cp sn Nothing Nothing (Just at)


findInstLabelId' :: GS1CompanyPrefix
                -> SerialNumber
                -> Maybe SGTINFilterValue
                -> Maybe ItemReference
                -> Maybe AssetType
                -> AppM (Maybe SB.PrimaryKeyType)
findInstLabelId' cp sn msfv mir mat = do
  r <- runDb $ runSelectReturningList $ select $ do
    labels <- all_ (SB._labels SB.supplyChainDb)
    guard_ (SB.label_gs1_company_prefix labels ==. val_ cp &&.
            SB.serial_number labels ==. (val_ $ Just sn) &&.
            (SB.sgtin_filter_value labels) ==. (val_ $ T.pack . show <$> msfv) &&.
            SB.asset_type labels ==. val_ mat &&.
            SB.item_reference labels ==. val_ mir)
    pure labels
  sandwichLog r
  case r of
    Right [l] -> return $ Just (SB.label_id l)
    _         -> return Nothing


findClassLabelId :: ClassLabelEPC -> AppM (Maybe SB.PrimaryKeyType)
findClassLabelId (LGTIN cp ir lot)  = findClassLabelId' cp Nothing ir (Just lot)
findClassLabelId (CSGTIN cp msfv ir) = findClassLabelId' cp msfv ir Nothing

findClassLabelId' :: GS1CompanyPrefix
                 -> Maybe SGTINFilterValue
                 -> ItemReference
                 -> Maybe Lot
                 -> AppM (Maybe SB.PrimaryKeyType)
findClassLabelId' cp msfv ir lot = do
  r <- runDb $ runSelectReturningList $ select $ do
    labels <- all_ (SB._labels SB.supplyChainDb)
    guard_ (
             SB.label_gs1_company_prefix labels ==. val_ cp &&.
             (SB.sgtin_filter_value labels) ==. (val_ $ T.pack . show <$> msfv) &&.
             SB.lot labels ==. (val_ lot) &&.
             SB.item_reference labels ==. (val_ . Just $ ir)
           )
    pure labels
  case r of
    Right [l] -> return $ Just (SB.label_id l)
    _         -> return Nothing


findLabelId :: LabelEPC -> AppM (Maybe SB.PrimaryKeyType)
findLabelId (IL l) = findInstLabelId l
findLabelId (CL c _) = findClassLabelId c

getParentId :: DWhat -> AppM (Maybe SB.PrimaryKeyType)
getParentId (TransactionDWhat _ (Just p) _ _) = findInstLabelId p
getParentId (AggregationDWhat _ (Just p) _)   = findInstLabelId p
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
    (printURI <$> mBiz)
    (printURI <$> mDisp)
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
            -> AppM SB.PrimaryKeyType
insertDWhat mBizTranId dwhat eventId = do
  pKey <- generatePk
  mParentId <- getParentId dwhat
  r <- runDb $ B.runInsert $ B.insert (SB._whats SB.supplyChainDb)
             $ insertValues
             [toStorageDWhat pKey mParentId mBizTranId eventId dwhat]
  case r of
    Left  e -> throwUnexpectedDBError $ sqlToServerError e
    Right _ -> return pKey

insertDWhen :: DWhen -> SB.PrimaryKeyType -> AppM SB.PrimaryKeyType
insertDWhen dwhen eventId =  do
  pKey <- generatePk
  r <- runDb $ B.runInsert $ B.insert (SB._whens SB.supplyChainDb)
             $ insertValues [toStorageDWhen pKey dwhen eventId]
  case r of
    Left  e -> throwUnexpectedDBError $ sqlToServerError e
    Right _ -> return pKey


insertDWhy :: DWhy -> SB.PrimaryKeyType -> AppM SB.PrimaryKeyType
insertDWhy dwhy eventId = do
  pKey <- generatePk
  r <- runDb $ B.runInsert $ B.insert (SB._whys SB.supplyChainDb)
             $ insertValues [toStorageDWhy pKey dwhy eventId]
  case r of
    Left  e -> throwUnexpectedDBError $ sqlToServerError e
    Right _ -> return pKey

insertSrcDestType :: SB.LocationField
                  -> SB.PrimaryKeyType
                  -> SrcDestLocation
                  -> AppM SB.PrimaryKeyType
insertSrcDestType
  locField
  eventId
  (sdType, SGLN gs1Company (LocationReference locationRef) ext) = do
  pKey <- generatePk
  let
      stWhere = SB.Where pKey
                (Just . toText $ sdType)
                locationRef
                (toText locField)
                (SB.EventId eventId)
  r <- runDb $ B.runInsert $ B.insert (SB._wheres SB.supplyChainDb)
             $ insertValues [stWhere]
  case r of
    Left  e -> throwUnexpectedDBError $ sqlToServerError e
    Right _ -> return pKey

insertLocationEPC :: SB.LocationField
                  -> SB.PrimaryKeyType
                  -> LocationEPC
                  -> AppM SB.PrimaryKeyType
insertLocationEPC
  locField
  eventId
  (SGLN gs1Company (LocationReference locationRef) ext) = do
  pKey <- generatePk
  let
      stWhere = SB.Where pKey
                Nothing
                locationRef
                (toText locField)
                (SB.EventId eventId)
  r <- runDb $ B.runInsert $ B.insert (SB._wheres SB.supplyChainDb)
             $ insertValues [stWhere]
  case r of
    Left  e -> throwUnexpectedDBError $ sqlToServerError e
    Right _ -> return pKey

-- | Maps the relevant insert function for all
-- ReadPoint, BizLocation, Src, Dest
insertDWhere :: DWhere -> SB.PrimaryKeyType -> AppM ()
insertDWhere (DWhere rPoint bizLoc srcT destT) eventId = do
  return $ insertLocationEPC SB.ReadPoint eventId <$> rPoint
  return $ insertLocationEPC SB.BizLocation eventId <$> bizLoc
  return $ insertSrcDestType SB.Src eventId <$> srcT
  return $ insertSrcDestType SB.Dest eventId <$> destT
  return ()

insertEvent :: SB.PrimaryKeyType -> T.Text -> Event -> AppM SB.PrimaryKeyType
insertEvent userId jsonEvent event = do
  pKey <- generatePk
  r <- runDb $ B.runInsert $ B.insert (SB._events SB.supplyChainDb)
             $ insertValues
             [toStorageEvent pKey userId jsonEvent (_eid event)]
  case r of
    Left  e -> throwUnexpectedDBError $ sqlToServerError e
    Right _ -> return pKey

insertUserEvent :: SB.PrimaryKeyType
                -> SB.PrimaryKeyType
                -> SB.PrimaryKeyType
                -> Bool
                -> (Maybe ByteString)
                -> AppM ()
insertUserEvent eventId userId addedByUserId signed signedHash = do
  pKey <- generatePk
  runDb $ B.runInsert $ B.insert (SB._user_events SB.supplyChainDb)
        $ insertValues
        [
          SB.UserEvent pKey
          (SB.EventId eventId)
          (SB.UserId userId)
          signed
          (SB.UserId addedByUserId)
          signedHash
        ]
  return ()

insertWhatLabel :: SB.PrimaryKeyType
                -> SB.PrimaryKeyType
                -> AppM SB.PrimaryKeyType
insertWhatLabel whatId labelId = do
  pKey <- generatePk
  runDb $ B.runInsert $ B.insert (SB._what_labels SB.supplyChainDb)
        $ insertValues
        [
          SB.WhatLabel pKey
          (SB.WhatId whatId)
          (SB.LabelId labelId)
        ]
  return pKey

-- | Given the necessary information,
-- converts a ``LabelEPC`` to SB.Label and writes it to the database
insertLabel :: Maybe T.Text
            -> SB.PrimaryKeyType
            -> LabelEPC
            -> AppM SB.PrimaryKeyType
insertLabel labelType whatId labelEpc = do
  pKey <- generatePk
  runDb $ B.runInsert $ B.insert (SB._labels SB.supplyChainDb)
        $ insertValues
        [ epcToStorageLabel labelType whatId pKey labelEpc]
  return pKey

-- | Ties up a label and an event entry in the database
insertLabelEvent :: SB.PrimaryKeyType
                 -> SB.PrimaryKeyType
                 -> AppM SB.PrimaryKeyType
insertLabelEvent eventId labelId = do
  pKey <- generatePk
  runDb $ B.runInsert $ B.insert (SB._label_events SB.supplyChainDb)
        $ insertValues
        [
          SB.LabelEvent pKey
          (SB.LabelId labelId)
          (SB.EventId eventId)
        ]
  return pKey

startTransaction :: AppM ()
startTransaction = do
  conn <- getDBConn
  liftIO $ begin conn

endTransaction :: AppM ()
endTransaction = do
  conn <- getDBConn
  liftIO $ execute_ conn "end transaction;" >> return ()

selectUser :: M.UserID -> AppM (Maybe SB.User)
selectUser uid = do
  r <- runDb $
          runSelectReturningList $ select $ do
          user <- all_ (SB._users SB.supplyChainDb)
          guard_ (SB.user_id user ==. val_ uid)
          pure user
  case r of
    Right [user] -> return $ Just user
    _            -> return Nothing

getEventList :: SB.PrimaryKeyType -> AppM [Ev.Event]
getEventList labelId = do
  r <- runDb $ 
        runSelectReturningList $ select $ do
        labelEvent <- all_ (SB._label_events SB.supplyChainDb)
        guard_ (SB.label_event_label_id labelEvent ==. (val_ $ SB.LabelId labelId))
        pure labelEvent
  case r of
    Left e -> throwUnexpectedDBError $ sqlToServerError e
  -- extract eventIds out
    Right labelEvents ->
      let
        eventIds = (SB.unEventId . SB.label_event_event_id) <$> labelEvents
        allEvents = sequence $ findEvent <$> eventIds
        in
          catMaybes <$> allEvents

findEvent :: SB.PrimaryKeyType -> AppM (Maybe Ev.Event)
findEvent eventId = do
  r <- runDb $ 
        runSelectReturningList $ select $ do
        event <- all_ (SB._events SB.supplyChainDb)
        guard_ (SB.event_id event ==. (val_ eventId))
        pure event
  case r of
    Right [event] -> return $ storageToModelEvent event
    Left  e       -> throwUnexpectedDBError $ sqlToServerError e
    _             -> throwBackendError r

storageToModelEvent :: SB.Event -> Maybe Ev.Event
storageToModelEvent = decodeEvent . SB.json_event 


-- | This is a test util to check that BEAM can insert and return time
-- insertTime :: AppM ()
-- insertTime = do
--   timeId <- generatePk
--   timeStamp <- generateTimeStamp
--   sandwichLog timeStamp
--   debugLog ("The program did not crash yet" )
--   r <- runDb $
--         runInsertReturningList (SB._my_time SB.supplyChainDb) $
--                insertValues
--                [
--                  SB.MyTime
--                  timeId
--                  timeStamp
--                ]
--   debugLog "Done with the query"
--   sandwichLog r

