{-# LANGUAGE OverloadedStrings #-}

-- | Following are a bunch of utility functions to do household stuff like
-- generating primary keys, timestamps - stuff that almost every function
-- below would need to do anyway
module StorageUtils where

import           Data.Time.LocalTime (utc, utcToLocalTime
                                     , LocalTime, localTimeToUTC
                                     , timeZoneOffsetString)
import           Data.Time (UTCTime)
import           AppConfig (AppM(..))
import qualified StorageBeam as SB
import           Data.UUID.V4 (nextRandom)
import           Data.Time.Clock (getCurrentTime)
import           Control.Monad.Reader (liftIO)
import           Data.GS1.Event (Event(..))
import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text.Lazy as TxtL
import qualified Data.Text as T
import qualified Model as M
import           Data.GS1.EPC
import           Data.GS1.DWhat (DWhat(..), LabelEPC(..))
import           Data.GS1.DWhy (DWhy(..))
import           Data.GS1.DWhere (DWhere(..), SrcDestLocation)
import           Data.GS1.DWhen (DWhen(..))
import qualified Data.GS1.EventID as EvId
import           Data.GS1.Event (Event(..), EventType(..))



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

-- should ``type`` be a maybe?
epcToStorageLabel :: EventType -> SB.PrimaryKeyType -> LabelEPC -> SB.Label
epcToStorageLabel evT pKey (IL (GIAI cp sn))         = error "not implemented yet" -- SB.Label pKey "GIAI" cp Nothing sn Nothing Nothing
epcToStorageLabel evT pKey (IL (SSCC cp sn))         = error "not implemented yet" -- SB.Label pKey "SSCC" cp Nothing sn Nothing Nothing
epcToStorageLabel evT pKey (IL (SGTIN cp fv ir sn))  = error "not implemented yet" -- SB.Label pKey "SGTIN" cp ir sn Nothing Nothing
epcToStorageLabel evT pKey (IL (GRAI cp at sn))      = error "not implemented yet" -- SB.Label pKey "GRAI" cp Nothing sn Nothing Nothing
epcToStorageLabel evT pKey (CL (LGTIN cp ir lot) mQ) = error "not implemented yet" -- SB.Label pKey "LGTIN" cp ir Nothing Nothing lot
epcToStorageLabel evT pKey (CL (CSGTIN cp fv ir) mQ) = error "not implemented yet" -- SB.Label pKey "CSGTIN" cp ir Nothing Nothing Nothing

-- | GS1 DWhat to Storage DWhat
-- For an object event
toStorageDWhat :: SB.PrimaryKeyType
               -> DWhat
               -> SB.PrimaryKeyType
               -> SB.What
toStorageDWhat pKey (ObjectDWhat act epcs) = error "not implemented yet"


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

