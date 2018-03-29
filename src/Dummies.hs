{-# LANGUAGE OverloadedStrings #-}

-- | Sample data types. Serves as a good example of the types defined
-- in GS1Combinators
module Dummies where

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import qualified Data.GS1.Event  as Ev
import           Data.Maybe      (fromJust)
import           Data.Time
import           Data.UUID       (nil)
import qualified Model           as M
import qualified StorageBeam     as SB

-- add function to generate and take dummyLabelEpc

-- General Utils
dummyNewUser :: M.NewUser
dummyNewUser = makeDummyNewUser "fake@gmail.com"

-- | Utility function to make many users on the fly
makeDummyNewUser :: M.Email -> M.NewUser
makeDummyNewUser emailAddress =
    M.NewUser "000" emailAddress "Bob" "Smith" "blah Ltd" "password"

dummyLocation :: LocationEPC
dummyLocation = SGLN "blah Ltd" (LocationReference "11111") Nothing

sampleObjectFile :: FilePath
sampleObjectFile = "../GS1Combinators/test/test-xml/ObjectEvent.xml"

dummyUser :: M.User
dummyUser = M.User nil "Sajid" "Anower"

dummyRsaPubKey :: M.RSAPublicKey
dummyRsaPubKey = M.PEMString "blah"

dummyId :: SB.PrimaryKeyType
dummyId = nil

dummyEpcList :: [LabelEPC]
dummyEpcList =
  [
    dummyLabelEpc,
    IL (SGTIN "0614141" Nothing "107346" "2018"), -- an extra label
    dummyClassLabel
  ]

dummyInstanceLabel :: InstanceLabelEPC
dummyInstanceLabel = SGTIN "0614141" (Just UnitLoad) "107346" "2017"


dummyClassLabel :: LabelEPC
dummyClassLabel = CL (CSGTIN "4012345" Nothing "098765") Nothing


dummyLabelEpc :: LabelEPC
dummyLabelEpc = IL dummyInstanceLabel

dummyParentLabel :: Maybe ParentLabel
dummyParentLabel = Just (SSCC "0614141" "1234567890")

dummyBizTransaction :: BizTransaction
dummyBizTransaction = BizTransaction{_btid="12345", _bt=Bol}
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
    dummyEpcList

dummyObject :: M.ObjectEvent
dummyObject = fromJust $ M.mkObjectEvent dummyObjEvent


-- Aggregation Events
dummyAggDWhat :: DWhat
dummyAggDWhat =
  AggregationDWhat
    Delete
    dummyParentLabel
    dummyEpcList

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

dummyTransactDWhat :: DWhat
dummyTransactDWhat =
  (TransactionDWhat
    Add
    dummyParentLabel
    [dummyBizTransaction]
    dummyEpcList
  )

dummyTransactEvent :: Ev.Event
dummyTransactEvent =
  Ev.Event
    Ev.TransactionEventT
    Nothing
    dummyTransactDWhat
    dummyDWhen
    dummyDWhy
    dummyDWhere

dummyTransaction :: M.TransactionEvent
dummyTransaction = fromJust $ M.mkTransactEvent dummyTransactEvent


-- Transformation Events

dummyTransfDWhat :: DWhat
dummyTransfDWhat =
  (TransformationDWhat
    Nothing
    dummyEpcList
    [CL (CSGTIN "4012345" Nothing "098769") Nothing]
    -- ^ adding a slightly different class for variety
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
    [
      (SDOwningParty,
      SGLN "0012347" (LocationReference "12345") Nothing)
    ]
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
