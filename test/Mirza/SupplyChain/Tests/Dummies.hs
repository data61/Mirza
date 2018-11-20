
-- | Sample data types. Serves as a good example of the types defined
-- in GS1Combinators
module Mirza.SupplyChain.Tests.Dummies where

import           Mirza.SupplyChain.Types hiding (NewUser (..))
import qualified Mirza.SupplyChain.Types as ST

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import qualified Data.GS1.Event          as Ev

import           Data.Maybe              (fromJust)
import qualified Data.Text               as T
import           Data.Time
import           Data.UUID               (nil)

import           Text.Email.Validate     (emailAddress)

import           Data.List.NonEmpty      (NonEmpty)

-- General Utils
dummyNewUser :: ST.NewUser
dummyNewUser = makeDummyNewUser (fromJust $ emailAddress "fake@gmail.com")

-- | Utility function to make many users on the fly
makeDummyNewUser :: EmailAddress -> ST.NewUser
makeDummyNewUser userEmail =
    ST.NewUser "000" userEmail "Bob" "Smith" (GS1CompanyPrefix "blah Ltd") "password"

dummyLocation :: LocationEPC
dummyLocation = SGLN (GS1CompanyPrefix "blah Ltd") (LocationReference "11111") Nothing

sampleObjectFile :: FilePath
sampleObjectFile = "../GS1Combinators/test/test-xml/ObjectEvent.xml"

dummyUser :: User
dummyUser = User (UserId dummyId) "Sajid" "Anower"

dummyId :: ST.PrimaryKeyType
dummyId = nil

dummyEpcList :: [LabelEPC]
dummyEpcList =
  [
    dummyLabelEpc,
    IL (SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2018")), -- an extra label
    dummyClassLabel
  ]

dummyInstanceLabel :: InstanceLabelEPC
dummyInstanceLabel = SGTIN (GS1CompanyPrefix "0614141") Nothing (ItemReference "107346") (SerialNumber "2017")


dummyClassLabel :: LabelEPC
dummyClassLabel = CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "098765")) Nothing

dummyLabelEpcUrn :: T.Text
dummyLabelEpcUrn = renderURL dummyInstanceLabel

dummyLabelEpc :: LabelEPC
dummyLabelEpc = IL dummyInstanceLabel

dummyParentLabel :: Maybe ParentLabel
dummyParentLabel = Just . ParentLabel $ (SSCC (GS1CompanyPrefix "0614141") (SerialNumber "1234567890"))

dummyBizTransaction :: BizTransaction
dummyBizTransaction = BizTransaction{_btid=BizTransactionId "12345", _bt=Bol}
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
  ObjWhat $
  ObjectDWhat
    Add
    dummyEpcList

dummyObject :: ObjectEvent
dummyObject = fromJust $ mkObjectEvent dummyObjEvent


-- Aggregation Events
dummyAggDWhat :: DWhat
dummyAggDWhat =
  AggWhat $
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

dummyAggregation :: AggregationEvent
dummyAggregation = fromJust $ mkAggEvent dummyAggEvent

-- Transaction Events

dummyTransactDWhat :: DWhat
dummyTransactDWhat =
  TransactWhat $
  TransactionDWhat
    Add
    dummyParentLabel
    [dummyBizTransaction]
    dummyEpcList

dummyTransactEvent :: Ev.Event
dummyTransactEvent =
  Ev.Event
    Ev.TransactionEventT
    Nothing
    dummyTransactDWhat
    dummyDWhen
    dummyDWhy
    dummyDWhere

dummyTransaction :: NonEmpty UserId -> TransactionEvent
dummyTransaction otherUserIds = fromJust $ mkTransactEvent dummyTransactEvent otherUserIds


-- Transformation Events

dummyTransfDWhat :: DWhat
dummyTransfDWhat =
  TransformWhat $
  TransformationDWhat
    Nothing
    (InputEPC <$> dummyEpcList)
    [OutputEPC $ CL (CSGTIN (GS1CompanyPrefix "4012345") Nothing (ItemReference "098769")) Nothing]
    -- adding a slightly different class for variety


dummyTransfEvent :: Ev.Event
dummyTransfEvent =
  Ev.Event
    Ev.TransformationEventT
    Nothing
    dummyTransfDWhat
    dummyDWhen
    dummyDWhy
    dummyDWhere

dummyTransformation :: TransformationEvent
dummyTransformation = fromJust $ mkTransfEvent dummyTransfEvent

-- Dimensions

dummyDWhen :: DWhen
dummyDWhen =
  DWhen
    (EPCISTime (read "2013-06-08 14:58:56.591+02:00" :: UTCTime))
    Nothing
    (read "+02:00" :: TimeZone)

dummyDWhere :: DWhere
dummyDWhere =
  DWhere
    [ReadPointLocation $ SGLN (GS1CompanyPrefix "0012345") (LocationReference "11111") (Just $ SGLNExtension "400")]
    -- [ReadPointLocation]
    [BizLocation $ SGLN (GS1CompanyPrefix "0012345") (LocationReference "11111") Nothing]
    -- [BizLocation]
    [
      SrcDestLocation (SDOwningParty,
      SGLN (GS1CompanyPrefix "0012347") (LocationReference "12345") Nothing)
    ]
    [
      SrcDestLocation (SDPossessingParty,
      SGLN (GS1CompanyPrefix "0012348") (LocationReference "12346") (Just $ SGLNExtension "400"))
    ]

dummyDWhy :: DWhy
dummyDWhy = DWhy (Just Receiving) (Just InProgress)
