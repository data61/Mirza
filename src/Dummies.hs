{-# LANGUAGE OverloadedStrings #-}

-- | Sample data types. Serves as a good example of the types defined
-- in GS1Combinators
module Dummies where

import           Data.UUID (nil)
import           Data.Maybe (fromJust)
import           Data.GS1.DWhat
import           Data.GS1.DWhy
import           Data.GS1.DWhere
import           Data.GS1.DWhen
import           Data.GS1.EPC
import qualified Data.GS1.Event as Ev
import           Data.Time.LocalTime
import           Data.Time
import qualified Model as M

-- add function to generate and take dummyLabelEpc

-- General Utils
dummyNewUser :: M.NewUser
dummyNewUser = M.NewUser "000" "fake@gmail.com" "New" "User" "Lomondo" "thi$i5fake"

-- | Utility function to make many users on the fly
makeDummyNewUser :: M.Email -> M.NewUser
makeDummyNewUser emailAddress = M.NewUser "000" emailAddress "Bob" "Smith" "blah Ltd" "password"

dummyLocation :: LocationEPC
dummyLocation = SGLN "blah Ltd" (LocationReference "11111") Nothing

sampleObjectFile :: FilePath
sampleObjectFile = "../GS1Combinators/test/test-xml/ObjectEvent.xml"

dummyUser :: M.User
dummyUser = M.User nil "Sajid" "Anower"

dummyRsaPubKey :: M.RSAPublicKey
dummyRsaPubKey = M.PEMString "blah"

-- Events

-- Object Events
dummyObjEvent :: Ev.Event
dummyObjEvent =
  Ev.Event
    Ev.ObjectEventT
    Nothing
    dummyObjectDWhat
    dummyDWhen
    dummyDWhy
    dummyDWhere

dummyObjectDWhat :: DWhat
dummyObjectDWhat =
  ObjectDWhat
    Add
    [
      dummyLabelEpc,
      IL (SGTIN "0614141" Nothing "107346" "2018")
    ]

dummyObject :: M.ObjectEvent
dummyObject = fromJust $ M.mkObjectEvent dummyObjEvent


-- Aggregation Events
dummyAggDWhat :: DWhat
dummyAggDWhat =
  AggregationDWhat
    Delete
    dummyParentLabel
    [
      dummyLabelEpc,
      IL (SGTIN "0614141" (Just UnitLoad) "107346" "2018")
    ]

dummyAggEvent :: Ev.Event
dummyAggEvent =
  Ev.Event
    Ev.AggregationEventT
    Nothing
    dummyAggDWhat
    dummyDWhen
    dummyDWhy
    dummyDWhere

dummyAggregation :: M.AggregationEvent
dummyAggregation = fromJust $ M.mkAggEvent dummyAggEvent

-- Transaction Events
-- Nothing here as of yet

-- Transformation Events

dummyTransfDWhat :: DWhat
dummyTransfDWhat =
  (TransformationDWhat
    Nothing
    [
      dummyLabelEpc,
      IL (SGTIN "0614141" Nothing "107346" "2018")
    ]
    [CL (CSGTIN "4012345" Nothing "098765") Nothing]
  )

dummyTransfEvent :: Ev.Event
dummyTransfEvent =
  Ev.Event
    Ev.TransformationEventT
    Nothing
    dummyTransfDWhat
    dummyDWhen
    dummyDWhy
    dummyDWhere

dummyTransformation :: M.TransformationEvent
dummyTransformation = fromJust $ M.mkTransfEvent dummyTransfEvent

-- Dimensions

dummyParentLabel :: Maybe ParentLabel
dummyParentLabel = Just (SSCC "0614141" "1234567890")

dummyLabelEpc :: LabelEPC
dummyLabelEpc = IL (SGTIN "0614141" (Just UnitLoad) "107346" "2017")

dummyDWhen :: DWhen
dummyDWhen =
  DWhen
    (read "2013-06-08 14:58:56.591+02:00" :: UTCTime)
    Nothing
    (read "+02:00" :: TimeZone)

dummyDWhere :: DWhere
dummyDWhere =
  DWhere
    [SGLN "0012345" (LocationReference "11111") (Just "400")]
    -- [ReadPointLocation]
    [SGLN "0012345" (LocationReference "11111") Nothing]
    -- [BizLocation]
    [(SDOwningParty, SGLN "0012347" (LocationReference "12345") Nothing)]
    [
      (SDPossessingParty,
      SGLN "0012348" (LocationReference "12346") (Just "400"))
    ]

dummyDWhy :: DWhy
dummyDWhy = DWhy (Just Receiving) (Just InProgress)


-- | @INCOMPLETE@ Utility function to read an XML and write that to database
-- runEventCreateObject :: FilePath -> AppM ()
-- runEventCreateObject xmlFile = do
--   doc <- liftIO $ Text.XML.readFile def xmlFile
--   let mainCursor = fromDocument doc
--       allParsedEvents =
--         filter (not . null) $ concat $
--         parseEventByType mainCursor <$> Ev.allEventTypes
--       (Right objEvent) = head allParsedEvents
--   eventId <- BQ.insertObjectEvent dummyUser dummyObject
--   liftIO $ print eventId
