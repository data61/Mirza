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
import           Data.Time (UTCTime)
import           AppConfig (AppM(..), runDb, getDBConn)
import qualified StorageBeam as SB
import           Data.UUID.V4 (nextRandom)
import           Data.Time.Clock (getCurrentTime)
import           Control.Monad.Reader (liftIO)
import           Data.GS1.Event (Event(..))
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as TxtL
import qualified Data.Text as T
import qualified Model as M
import           Data.GS1.EPC as EPC
import           Data.GS1.DWhat (DWhat(..), LabelEPC(..))
import           Data.GS1.DWhy (DWhy(..))
import           Data.GS1.DWhere (DWhere(..), SrcDestLocation)
import           Data.GS1.DWhen (DWhen(..))
import qualified Data.GS1.EventID as EvId
import           Data.GS1.Event (Event(..), EventType(..),
                                evTypeToTextLike, dwhatToEventTextLike)
import           Utils (toText, debugLog)
import           Database.Beam as B
import           Data.ByteString (ByteString)
import           ErrorUtils (throwBackendError, throwAppError, toServerError
                            , defaultToServerError, sqlToServerError
                            , throwUnexpectedDBError)
import           Database.PostgreSQL.Simple

-- | Reads back the ``LocalTime`` in UTCTime (with an offset of 0)
toEPCISTime :: LocalTime -> UTCTime
toEPCISTime = localTimeToUTC utc

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
toLocalTime :: UTCTime -> LocalTime
toLocalTime = utcToLocalTime utc

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

getQuantityAmount :: Maybe Quantity -> Maybe Double
getQuantityAmount Nothing = Nothing
getQuantityAmount (Just (MeasuredQuantity a _)) = Just $ realToFrac a
getQuantityAmount (Just (ItemCount c)) = Just $ realToFrac c

getQuantityUom :: Maybe Quantity -> Maybe EPC.Uom
getQuantityUom Nothing = Nothing
getQuantityUom (Just (MeasuredQuantity _ u)) = Just u
getQuantityUom (Just (ItemCount _)) = Nothing

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

-- | GS1 DWhat to Storage DWhat
-- For an object event
toStorageDWhat :: SB.PrimaryKeyType
               -> Maybe SB.PrimaryKeyType
               -> Maybe SB.PrimaryKeyType
               -> Maybe SB.PrimaryKeyType
               -> SB.PrimaryKeyType
               -> DWhat
               -> SB.What
toStorageDWhat pKey mParentId mBizTranId mTranId eventId dwhat
   = SB.What pKey
            (Just $ dwhatToEventTextLike dwhat)
            (toText <$> getAction dwhat)
            (SB.LabelId mParentId)
            (SB.BizTransactionId mBizTranId)
            (SB.TransformationId mTranId)
            (SB.EventId eventId)

getAction :: DWhat -> Maybe Action
getAction (TransformationDWhat _ _ _) = Nothing
getAction (ObjectDWhat act _) = Just act
getAction (TransactionDWhat act _ _ _) = Just act
getAction (AggregationDWhat act _ _) = Just act

grabInstLabelId :: GS1CompanyPrefix
                -> SerialNumber
                -> Maybe SGTINFilterValue
                -> Maybe ItemReference
                -> Maybe AssetType
                -> AppM (Maybe SB.PrimaryKeyType)
grabInstLabelId cp sn msfv mir mat = do
  r <- runDb $ runSelectReturningList $ select $ do
    labels <- all_ (SB._labels SB.supplyChainDb)
    guard_ (SB.label_gs1_company_prefix labels ==. val_ cp &&.
            SB.serial_number labels ==. (val_ $ Just sn) &&.
            (SB.sgtin_filter_value labels) ==. (val_ $ T.pack . show <$> msfv) &&.
            SB.asset_type labels ==. val_ mat &&.
            SB.item_reference labels ==. val_ mir)
    pure labels
  case r of
    Right [l] -> return $ Just (SB.label_id l)
    _         -> return Nothing

findLabelId :: InstanceLabelEPC -> AppM (Maybe SB.PrimaryKeyType)
findLabelId (GIAI cp sn) = grabInstLabelId cp sn Nothing Nothing Nothing
findLabelId (SSCC cp sn) = grabInstLabelId cp sn Nothing Nothing Nothing
findLabelId (SGTIN cp msfv ir sn) = grabInstLabelId cp sn msfv (Just ir) Nothing
findLabelId (GRAI cp at sn) = grabInstLabelId cp sn Nothing Nothing (Just at)

getParentId :: DWhat -> AppM (Maybe SB.PrimaryKeyType)
getParentId (TransactionDWhat _ (Just p) _ _) = findLabelId p
getParentId (AggregationDWhat _ (Just p) _)   = findLabelId p
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
            -> Maybe SB.PrimaryKeyType
            -> DWhat
            -> SB.PrimaryKeyType
            -> AppM SB.PrimaryKeyType
insertDWhat mBizTranId mTranId dwhat eventId = do
  pKey <- generatePk
  mParentId <- getParentId dwhat
  r <- runDb $ B.runInsert $ B.insert (SB._whats SB.supplyChainDb)
             $ insertValues
             [toStorageDWhat pKey mParentId mBizTranId mTranId eventId dwhat]
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
    Left  e -> do
      debugLog "Woah"
      throwUnexpectedDBError $ sqlToServerError e
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

insertLabel :: LabelEPC
            -> Maybe T.Text
            -> SB.PrimaryKeyType
            -> AppM SB.PrimaryKeyType
insertLabel labelEpc labelType whatId = do
  pKey <- generatePk
  runDb $ B.runInsert $ B.insert (SB._labels SB.supplyChainDb)
        $ insertValues
        [ epcToStorageLabel labelType whatId pKey labelEpc ]
  return pKey

startTransaction :: AppM ()
startTransaction = do
  conn <- getDBConn
  liftIO $ begin conn

endTransaction :: AppM ()
endTransaction = do
  conn <- getDBConn
  liftIO $ execute_ conn "end transaction;" >> return ()
