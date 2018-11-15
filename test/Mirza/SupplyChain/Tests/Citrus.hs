{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Tests.Citrus where

import           Control.Exception                      (bracket)

import           Mirza.BusinessRegistry.Client.Servant  as BRClient

import           Mirza.Common.Tests.InitClient
import           Mirza.Common.Tests.ServantUtils        (runClient)
import           Mirza.SupplyChain.Tests.Generate

import           Servant.Client                         (ClientM)

import           Servant.API.BasicAuth                  (BasicAuthData (..))

-- import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Mirza.BusinessRegistry.Database.Schema (LocationId)
import           Mirza.BusinessRegistry.Types           as BT

-- import           Mirza.SupplyChain.Types                as ST

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event

import           Data.Time                              (TimeZone, addUTCTime)

import           Mirza.Common.Utils                     (toText)

{-

Tests that should be implemented here

Check Provenance of a labelEPC
where I've used head, you need to use map to actually do it for all elements in the list. I've just done one element for illustrative purposes.
eventList <- listEvents <labelEPC>
let event = head eventList
eventInfo <- eventInfo(eventID)
(sig, uid) = head (signatures eventInfo)
publicKey <- getPublicKey uid
assert $ decrypt(sig, publicKey) == (joseText eventInfo)


Get all events that relate to a labelEPC
eventList <- listEvents <labelEPC>
subEvents eventList = [e | e <- eventList, if
(eventType e == aggregationEvent || eventType e == transformationEvent)
then (map subEvents $ map listEvents (getSubEPCs e)]


-}

citrusSpec :: IO TestTree
citrusSpec = do
  let citrusSupplyChainTests = testCaseSteps "Creating food provenance trail" $ \step ->
        bracket runApps endApps $ \testData -> do

          let scsUrl = scsBaseUrl testData
              httpSCS = runClient scsUrl
              brUrl = brBaseUrl testData
              httpBR = runClient brUrl
              brAuthUser = brAuthData testData

          step "insert prelim data into SCS and BR"
          _userIdsSCS <- httpSCS scsUsers
          _gs1prefixes <- httpBR $ insertBusinesses brAuthUser businessList
          _locationIds <- httpBR $ insertLocations brAuthUser locationList

          -- step "insert the users into BR"
          -- userIdsBR <- httpBR brUsers

          step "insert citrus events into SCS, sign & counter sign them"
          -- for each event in CitrusEvents,
          -- insert key(s) into BR
          -- insert event into SCS
          -- sign (and countersign) using the keys you inserted, and create
          -- a SignedEvent. insert into SCS using eventSign.

          step "check eventInfo for each event"

          step "get all events related to boxLabel"
          error "not implemented yet"

  pure $ testGroup "Citrus Client tests"
        [ citrusSupplyChainTests
        ]


{- This is not a real list (obviously) but
I wrote it up to help implement the other functions.

citrusEntities :: []
citrusEntities =
  [regulator1,
   regulator2,
   farmer,
   (farmer, truckDriver1),
   (truckDriver1, packingHouseOperator),
   packingHouseOperator,
   packingHouseOperator,
   (packingHouseOperator, truckDriver2),
   (truckDriver2, portsOperator1),
   portsOperator1,
   (portsOperator1, shippingCompany),
   regulator3
  ]
-}

--TODO: Define the gs1CompanyIdentifiers used in the supply chain:
farmerCompanyPrefix :: GS1CompanyPrefix
farmerCompanyPrefix = GS1CompanyPrefix "1111"
truckDriver1CompanyPrefix :: GS1CompanyPrefix
truckDriver1CompanyPrefix = GS1CompanyPrefix "2222"
regulator1CompanyPrefix :: GS1CompanyPrefix
regulator1CompanyPrefix = GS1CompanyPrefix "3333"
regulator2CompanyPrefix :: GS1CompanyPrefix
regulator2CompanyPrefix = GS1CompanyPrefix "4444"
packingHouseCompanyPrefix :: GS1CompanyPrefix
packingHouseCompanyPrefix = GS1CompanyPrefix "5555"
auPortCompanyPrefix :: GS1CompanyPrefix
auPortCompanyPrefix = GS1CompanyPrefix "7777"
cnPortCompanyPrefix :: GS1CompanyPrefix
cnPortCompanyPrefix = GS1CompanyPrefix "8888"
truck2CompanyPrefix :: GS1CompanyPrefix
truck2CompanyPrefix = GS1CompanyPrefix "1212"
regulator3CompanyPrefix :: GS1CompanyPrefix
regulator3CompanyPrefix = GS1CompanyPrefix "4545"
regulator4CompanyPrefix :: GS1CompanyPrefix
regulator4CompanyPrefix = GS1CompanyPrefix "8989"

allPrefixes :: [GS1CompanyPrefix]
allPrefixes = [
    farmerCompanyPrefix
  , truckDriver1CompanyPrefix
  , regulator1CompanyPrefix
  , regulator2CompanyPrefix
  , packingHouseCompanyPrefix
  , auPortCompanyPrefix
  , cnPortCompanyPrefix
  , truck2CompanyPrefix
  , regulator3CompanyPrefix
  , regulator4CompanyPrefix ]

--TODO: Define the locations ... fill out the rest of these GLNs
farmLocation :: LocationEPC
farmLocation = SGLN farmerCompanyPrefix (LocationReference "1") Nothing -- "blockID3"
regulator1Biz :: LocationEPC
regulator1Biz = SGLN regulator1CompanyPrefix (LocationReference "1") Nothing
regulator2Biz :: LocationEPC
regulator2Biz = SGLN regulator2CompanyPrefix (LocationReference "1") Nothing
packingHouseLocation :: LocationEPC
packingHouseLocation = SGLN packingHouseCompanyPrefix (LocationReference "1") Nothing
auPortLocation :: LocationEPC
auPortLocation = SGLN auPortCompanyPrefix (LocationReference "1") Nothing
cnPortLocation :: LocationEPC
cnPortLocation = SGLN cnPortCompanyPrefix (LocationReference "1") Nothing
farmerBiz :: LocationEPC
farmerBiz = SGLN farmerCompanyPrefix (LocationReference "1") Nothing
packingHouseBiz :: LocationEPC
packingHouseBiz = SGLN packingHouseCompanyPrefix (LocationReference "1") Nothing
truck2Biz :: LocationEPC
truck2Biz = SGLN truck2CompanyPrefix (LocationReference "1") Nothing
regulator3Biz :: LocationEPC
regulator3Biz = SGLN regulator3CompanyPrefix (LocationReference "1") Nothing
regulator4Biz :: LocationEPC
regulator4Biz = SGLN regulator4CompanyPrefix (LocationReference "1") Nothing


allLocationEPC :: [LocationEPC]
allLocationEPC = [
    farmLocation
  , regulator1Biz
  , regulator2Biz
  , packingHouseLocation
  , auPortLocation
  , cnPortLocation
  , farmerBiz
  , packingHouseBiz
  , truck2Biz
  , regulator3Biz
  , regulator4Biz ]


--TODO: Create a list of NewLocations for insertion into the BR using
--the above GLNs.
locationList :: [NewLocation]
locationList = [
    NewLocation farmLocation (Just (Latitude 122.3, Longitude 123.9)) (Just "17 Cherry Drive, Young")
  , NewLocation regulator1Biz (Just (Latitude 192.3, Longitude 113.9)) (Just "NSW PestControl, Wyong")
  , NewLocation regulator2Biz (Just (Latitude 134.6, Longitude 126.9)) (Just "7 Citrus Street, Gordon")
  , NewLocation packingHouseLocation (Just (Latitude 102.3, Longitude 110.9)) (Just "14 Plucking Street, WoyWoy")
  , NewLocation auPortLocation (Just (Latitude 190.3, Longitude 115.8)) (Just "21 Pitkin Avenue, Muswellbrook")
  , NewLocation cnPortLocation (Just (Latitude 234.3, Longitude 137.8)) (Just "34 Park Boulevard, Merriwa")
  , NewLocation farmerBiz (Just (Latitude 291.3, Longitude 173.2)) (Just "23 Cleveland Street, Surry Hills")
  , NewLocation packingHouseBiz (Just (Latitude 182.5, Longitude 120.1)) (Just "141 Homer Street, Ashfield")
  , NewLocation truck2Biz (Just (Latitude 222.2, Longitude 112.1)) (Just "90 Crescent Road, Moss Vale")
  , NewLocation regulator3Biz (Just (Latitude 165.1, Longitude 114.6)) (Just "37 York Street")
  , NewLocation regulator4Biz (Just (Latitude 154.3, Longitude 119.9)) (Just "63 Chopin Street, Woolloomolloo")
  ]


--TODO: make a list of newBusinesses:
businessList :: [NewBusiness]
businessList = [
    NewBusiness farmerCompanyPrefix "farmer"
  , NewBusiness truckDriver1CompanyPrefix "truckDriver1"
  , NewBusiness regulator1CompanyPrefix "regulator1"
  , NewBusiness regulator2CompanyPrefix "regulator2"
  , NewBusiness packingHouseCompanyPrefix "packingHouse"
  , NewBusiness auPortCompanyPrefix "auPort"
  , NewBusiness cnPortCompanyPrefix "cnPort"
  , NewBusiness truck2CompanyPrefix "truck2"
  , NewBusiness regulator3CompanyPrefix "regulator3"
  , NewBusiness regulator4CompanyPrefix "regulator4"
  ]

--TODO: Write a function that given a list of GS1CompanyIdentifiers (gathered from
--the locations above and the labels below) and puts them in the BR via the BRClient
insertBusinesses :: BasicAuthData -> [NewBusiness] -> ClientM [GS1CompanyPrefix]
insertBusinesses brAuthUser bizList = sequence $ BRClient.addBusiness brAuthUser <$> bizList

--TODO: Write a function that given a list of GLNs, inserts them into the BRClient
insertLocations :: BasicAuthData -> [NewLocation] -> ClientM [LocationId]
insertLocations brAuthUser locs = sequence $ BRClient.addLocation brAuthUser <$> locs


--TODO: Create enough key pairs for all the supply chain entities
--and save them in the TestData/Citrus dir


-- All the labels that feed into citrusEvents
landLabel :: InstanceLabelEPC
landLabel = GRAI farmerCompanyPrefix (AssetType "blockLabel") (SerialNumber "88")
binLabel :: InstanceLabelEPC
binLabel = GIAI farmerCompanyPrefix (SerialNumber "1")
truckLabel :: InstanceLabelEPC
truckLabel = SSCC truckDriver1CompanyPrefix (SerialNumber "1")
boxLabel :: InstanceLabelEPC
boxLabel = GIAI farmerCompanyPrefix (SerialNumber "1")
palletLabel :: InstanceLabelEPC
palletLabel = GRAI packingHouseCompanyPrefix (AssetType "palletLabel") (SerialNumber "1")
truck2Label :: InstanceLabelEPC
truck2Label = SSCC truck2CompanyPrefix (SerialNumber "1")
shipLabel :: InstanceLabelEPC
shipLabel = SSCC cnPortCompanyPrefix (SerialNumber "23")
binLabels :: [LabelEPC]
binLabels = [IL binLabel, IL $ GIAI farmerCompanyPrefix (SerialNumber "2")]
boxLabels :: [LabelEPC]
boxLabels = [IL boxLabel, IL $ GIAI farmerCompanyPrefix (SerialNumber "2")]
palletLabels :: [LabelEPC]
palletLabels = [IL palletLabel, IL $ GRAI packingHouseCompanyPrefix (AssetType "palletLabel") (SerialNumber "2")]

-- Create users in the SCS db. Need to also create them in
-- the BR. This should be re-implemented as a client fucntion, so
-- you can do it the same way in both SCS and BR.
scsUsers :: ClientM [UserId]
scsUsers =
  let userNames = ["regulator1", "regulator2", "farmer", "truckDriver1", "packingHouseOperator", "truckDriver2", "portsOperator1", "shippingCompany", "regulator3", "regulator4"]
      nUsers = length userNames
      initPrefix = 11111111
      gs1companyPrefixes = map (GS1CompanyPrefix . toText) [initPrefix.. initPrefix+nUsers]
  in
  insertMultipleUsersSCS
    "citrusSupplyChain" userNames gs1companyPrefixes

-- Create a list of events starting at "startTime" in a particular
-- timezone.
citrusEvents :: EPCISTime -> TimeZone -> [Event]
citrusEvents startTime tz =
  [ pestControl [instanceLandLabel] startTime tz rpFarmLocation (BizLocation regulator1Biz),
    maxResidue [instanceLandLabel] (addEpcisTime startTime 1) tz rpFarmLocation (BizLocation regulator2Biz),
    labelBinsHarvest [instanceLandLabel] (addEpcisTime startTime 2) tz rpFarmLocation (BizLocation farmerBiz),
    farmerToTruckDriver1 parentTruckLabel binLabels (addEpcisTime startTime 3) tz rpFarmLocation (BizLocation farmerBiz),
    truckDriver1ToPackingHouse parentTruckLabel binLabels (addEpcisTime startTime 4) tz rpPackingHouseLocation locationPackingHouse,
    applyFungicide binLabels (addEpcisTime startTime 5) tz rpPackingHouseLocation locationPackingHouse,
    sortingBoxing (Just . ParentLabel $ boxLabel) binLabels (addEpcisTime startTime 6) tz rpPackingHouseLocation locationPackingHouse,
    palletisation (Just . ParentLabel $ palletLabel) boxLabels (addEpcisTime startTime 7) tz rpPackingHouseLocation locationPackingHouse,
    packingHouseToTruckDriver2 parentTruck2Label palletLabels (addEpcisTime startTime 8) tz rpPackingHouseLocation locationPackingHouse,
    truckDriver2ToPortsOperator1 parentTruck2Label palletLabels (addEpcisTime startTime 9) tz rpAuPort (BizLocation truck2Biz),
    quarantineAus palletLabels (addEpcisTime startTime 10) tz rpAuPort (BizLocation regulator3Biz),
    shippingToChina (Just . ParentLabel $ shipLabel) palletLabels (addEpcisTime startTime 11) tz rpCnPort (BizLocation cnPortLocation),
    quarantineChina palletLabels (addEpcisTime startTime 12) tz rpCnPort (BizLocation regulator4Biz)
  ]
  where
    addEpcisTime (EPCISTime currTime) toAdd = EPCISTime $ addUTCTime (toAdd * 60) currTime
    instanceLandLabel = IL landLabel
    rpFarmLocation = ReadPointLocation farmLocation
    rpPackingHouseLocation = ReadPointLocation packingHouseLocation
    parentTruckLabel = Just . ParentLabel $ truckLabel
    parentTruck2Label = Just . ParentLabel $ truck2Label
    locationPackingHouse = BizLocation packingHouseBiz
    rpAuPort = ReadPointLocation auPortLocation
    rpCnPort = ReadPointLocation cnPortLocation


-- A series of events in a citrus supply chain.

--pest control
pestControl :: [LabelEPC]
            -> EPCISTime
            -> TimeZone
            -> ReadPointLocation
            -> BizLocation
            -> Event
pestControl blockId t tz location bizLocation =
  Event ObjectEventT Nothing
          (ObjWhat $ ObjectDWhat Observe blockId)
          (DWhen t Nothing tz)
          (DWhy (Just Inspecting) (Just SellableNotAccessible))
          (DWhere [location] [bizLocation] [] [])

--check maximum residue of pesticides/fungicides
maxResidue :: [LabelEPC]
           -> EPCISTime
           -> TimeZone
           -> ReadPointLocation
           -> BizLocation
           -> Event
maxResidue blockId t tz location bizLocation =
  Event ObjectEventT Nothing
      (ObjWhat $ ObjectDWhat Observe blockId)
      (DWhen t Nothing tz)
      (DWhy (Just Inspecting) (Just SellableNotAccessible))
      (DWhere [location] [bizLocation] [] [])

--label bins/harvest
labelBinsHarvest :: [LabelEPC]
                 -> EPCISTime
                 -> TimeZone
                 -> ReadPointLocation
                 -> BizLocation
                 -> Event
labelBinsHarvest binId t tz location bizLocation =
  Event ObjectEventT Nothing
      (ObjWhat $ ObjectDWhat Add binId) -- is Add the right action here?
      (DWhen t Nothing tz)
      (DWhy (Just Commissioning) (Just Active))
      (DWhere [location] [bizLocation] [] [])

{- is this needed, or do we just make a transaction event with the parent
    being the truckID?
loadingTruckToPackingHouse :: [LabelEPC] -> LabelEPC -> EPCISTime -> TimeZone ->
  ReadPointLocation -> BizLocation -> Event
loadingTruckToPackingHouse binIds truckId t tz location bizLocation =
  Event AggregationEventT Nothing
  (AggWhat $ AggregationDWhat Add truckId binIds)
  (DWhen t Nothing tz)
  (DWhy (Just Loading) (Just SellableNotAccessible))
  (DWhere [location] [bizLocation] [] [])
  -}

--Transport
farmerToTruckDriver1 :: Maybe ParentLabel
                     -> [LabelEPC]
                     -> EPCISTime
                     -> TimeZone
                     -> ReadPointLocation
                     -> BizLocation
                     -> Event
farmerToTruckDriver1 mtruckId binIds t tz location bizLocation =
  Event TransactionEventT Nothing
  (TransactWhat $ TransactionDWhat Add mtruckId [] binIds)
  (DWhen t Nothing tz)
  (DWhy (Just Loading) (Just InTransit))
  (DWhere [location] [bizLocation] [] [])

--Scan bins at packing house
truckDriver1ToPackingHouse :: Maybe ParentLabel
                           -> [LabelEPC]
                           -> EPCISTime
                           -> TimeZone
                           -> ReadPointLocation
                           -> BizLocation
                           -> Event
truckDriver1ToPackingHouse truckId binIds t tz location bizLocation =
  Event TransactionEventT Nothing
  (TransactWhat $ TransactionDWhat Delete truckId [] binIds)
  (DWhen t Nothing tz)
  (DWhy (Just Accepting) (Just InProgress))
  (DWhere [location] [bizLocation] [] [])

--apply fungicide within 36 hours
applyFungicide :: [LabelEPC]
               -> EPCISTime
               -> TimeZone
               -> ReadPointLocation
               -> BizLocation
               -> Event
applyFungicide binIds t tz location bizLocation =
  Event TransformationEventT Nothing
  (TransformWhat $ TransformationDWhat Nothing (InputEPC <$> binIds) (OutputEPC <$> binIds))
  (DWhen t Nothing tz)
  (DWhy (Just Inspecting) (Just SellableNotAccessible))
  (DWhere [location] [bizLocation] [] [])

--sorting and boxing
sortingBoxing :: Maybe ParentLabel
              -> [LabelEPC]
              -> EPCISTime
              -> TimeZone
              -> ReadPointLocation
              -> BizLocation
              -> Event
sortingBoxing boxId contents t tz location bizLocation =
  Event AggregationEventT Nothing
  (AggWhat $ AggregationDWhat Add boxId contents)
  (DWhen t Nothing tz)
  (DWhy (Just Commissioning) (Just Active))
  (DWhere [location] [bizLocation] [] [])

-- palletisation
palletisation :: Maybe ParentLabel
              -> [LabelEPC]
              -> EPCISTime
              -> TimeZone
              -> ReadPointLocation
              -> BizLocation
              -> Event
palletisation palletId boxes t tz location bizLocation =
  Event AggregationEventT Nothing
  (AggWhat $ AggregationDWhat Add palletId boxes)
  (DWhen t Nothing tz)
  (DWhy (Just Commissioning) (Just Active))
  (DWhere [location] [bizLocation] [] [])


--loading onto truck
packingHouseToTruckDriver2 :: Maybe ParentLabel
                           -> [LabelEPC]
                           -> EPCISTime
                           -> TimeZone
                           -> ReadPointLocation
                           -> BizLocation
                           -> Event
packingHouseToTruckDriver2 truckId palletIds t tz location bizLocation =
  Event TransactionEventT Nothing
  (TransactWhat $ TransactionDWhat Add truckId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy (Just Loading) (Just InTransit))
  (DWhere [location] [bizLocation] [] [])

-- arrival of goods at the port
-- take them out of the truck
truckDriver2ToPortsOperator1 :: Maybe ParentLabel
                             -> [LabelEPC]
                             -> EPCISTime
                             -> TimeZone
                             -> ReadPointLocation
                             -> BizLocation
                             -> Event
truckDriver2ToPortsOperator1 truckId palletIds t tz location bizLocation =
  Event TransactionEventT Nothing
  (TransactWhat $ TransactionDWhat Delete truckId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy (Just Loading) (Just InTransit))
  (DWhere [location] [bizLocation] [] [])

-- quarantine in australia
-- transformed state from non-quarantined to quarantined
quarantineAus :: [LabelEPC]
              -> EPCISTime
              -> TimeZone
              -> ReadPointLocation
              -> BizLocation
              -> Event
quarantineAus palletIds t tz location bizLocation =
  Event TransformationEventT Nothing
  (TransformWhat $ TransformationDWhat Nothing (InputEPC <$> palletIds) (OutputEPC <$> palletIds))
  (DWhen t Nothing tz)
  (DWhy (Just Holding) (Just SellableNotAccessible))
  (DWhere [location] [bizLocation] [] [])

-- shipping to China
shippingToChina :: Maybe ParentLabel
                -> [LabelEPC]
                -> EPCISTime
                -> TimeZone
                -> ReadPointLocation
                -> BizLocation
                -> Event
shippingToChina shipId palletIds t tz location bizLocation =
  Event TransactionEventT Nothing
  (TransactWhat $ TransactionDWhat Delete shipId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy (Just Shipping) (Just InTransit))
  (DWhere [location] [bizLocation] [] [])

-- quarantine in China
-- transformed state from non-quarantined to quarantined
quarantineChina :: [LabelEPC]
                -> EPCISTime
                -> TimeZone
                -> ReadPointLocation
                -> BizLocation
                -> Event
quarantineChina palletIds t tz location bizLocation =
  Event TransformationEventT Nothing
  (TransformWhat $ TransformationDWhat Nothing (InputEPC <$> palletIds) (OutputEPC <$> palletIds))
  (DWhen t Nothing tz)
  (DWhy (Just Holding) (Just SellableNotAccessible))
  (DWhere [location] [bizLocation] [] [])
