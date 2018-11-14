{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Tests.Citrus where

import           Control.Exception                      (bracket)

import           Mirza.Common.Tests.InitClient
import           Mirza.Common.Tests.ServantUtils        (runClient)
import           Mirza.SupplyChain.Tests.Generate

import           Servant.Client                         (ClientM)

import           Servant.API.BasicAuth                  (BasicAuthData (..))

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Mirza.BusinessRegistry.Database.Schema (LocationId)
import           Mirza.BusinessRegistry.Types           as ST

import           Mirza.SupplyChain.Types                as ST

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event

import           Data.Time                              (TimeZone)

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
              brUrl = brBaseUrl testData
              httpSCS = runClient scsUrl
              httpBR = runClient brUrl
              brAuthUser = brAuthData testData

          step "insert prelim data into SCS and BR"
          userIdsSCS <- httpSCS scsUsers
          gs1prefixes <- insertBusinesses
          locationIds <- insertLocations

          -- step "insert the users into BR"
          -- userIdsBR <- httpBR brUsers

          step "insert citrus events into SCS, sign & counter sign them"
          -- iterate through the citrusEvents, insert them, and sign and countersign them

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


--TODO: Define the locations ... fill out the rest of these GLNs
farmLocation = SGLN farmerCompanyPrefix (LocationReference "1") "blockID3"
regulator1Biz = SGLN regulator1CompanyPrefix (LocationReference "1") Nothing
regulator2Biz = SGLN regulator2CompanyPrefix (LocationReference "1") Nothing
packingHouseLocation = SGLN packingHouseCompanyPrefix (LocationReference "1") Nothing
auPortLocation = SGLN auPortCompanyPrefix (LocationReference "1") Nothing
cnPortLocation = SGLN cnPortCompanyPrefix (LocationReference "1") Nothing
farmerBiz = SGLN farmerCompanyPrefix (LocationReference "1") Nothing
packingHouseBiz = SGLN packingHouseCompanyPrefix (LocationReference "1") Nothing
truck2Biz = SGLN truck2CompanyPrefix (LocationReference "1") Nothing
regulator3Biz = SGLN regulator3CompanyPrefix (LocationReference "1") Nothing
regulator4Biz = SGLN regulator4CompanyPrefix (LocationReference "1") Nothing


--TODO: Create a list of NewLocations for insertion into the BR using
--the above GLNs. The formatting below is illustrative only, I probably
--haven't defined the lat long correctly.
-- locationList :: [NewLocation]
-- locationList = [
--   (NewLocation farmLocation (122.3, 123.9) "17 Cherry Drive, Young"),
--   (NewLocation regulator1Biz (192.3, 1l3.9) "NSW PestControl, Wyong")
--   -- ...
--     ]

--TODO: Define the gs1CompanyIdentifiers used in the supply chain:
farmerCompanyPrefix = GS1CompanyPrefix "1111"
truckDriver1GS1CompanyPrefix = GS1CompanyPrefix "2222"
regulator1CompanyPrefix = GS1CompanyPrefix "3333"
regulator2CompanyPrefix = GS1CompanyPrefix "4444"
packingHouseCompanyPrefix = GS1CompanyPrefix "5555"
auPortCompanyPrefix = GS1CompanyPrefix "7777"
cnPortCompanyPrefix = GS1CompanyPrefix "8888"
truck2CompanyPrefix = GS1CompanyPrefix "1212"
regulator3CompanyPrefix = GS1CompanyPrefix "4545"
regulator4CompanyPrefix = GS1CompanyPrefix "8989"


--TODO: make a list of newBusinesses:
businessList :: [NewBusiness]
businessList = error "implement me"

--TODO: Write a function that given a list of GS1CompanyIdentifiers (gathered from
--the locations above and the labels below) and puts them in the BR via the BRClient
insertBusinesses :: BasicAuthData -> [NewBusiness] -> ClientM [GS1CopmanyPrefix]
insertBusinesses = error "implement me"

--TODO: Write a function that given a list of GLNs, inserts them into the BRClient
insertLocations :: [NewLocation] -> ClientM [LocationId]
insertLocations = error "implement me"


-- All the labels that feed into citrusEvents
landLabel = GRAI farmerGS1CompanyPrefix "blockLabel" (SerialNumber "88")
binLabel = GIAI farmerGS1CompanyPrefix "binLabel" (SerialNumber "1")
truckLabel = SSCC truckDriver1GS1CompanyPrefix (SerialNumber "1")
binLabels = [binLabel, GIAI farmerGS1CompanyPrefix "binLabel" (SerialNumber "2")]
boxLabel = GIAI farmerGS1CompanyPrefix "boxLabel" (SerialNumber "1")
palletLabel = GRAI packingHouseGS1CompanyPrefix "palletLabel" (SerialNumber "1")
boxLabels = [boxLabel, GIAI farmerGS1CompanyPrefix "boxLabel" (SerialNumber "2")]
palletLabels = [palletLabel, GRAI packingHouseGS1CompanyPrefix "palletLabel" (SerialNumber "2")]
truck2Label = SSCC truckDriver2GS1CompanyPrefix (SerialNumber "1")


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
citrusEvents startTime tz readPoints bizs =
  [pestControl landLabel startTime tz farmLocation regulator1Biz,
   maxResidue landLabel startTime+1 tz farmLocation regulator2Biz,
   labelBinsHarvest binLabel startTime+2 tz farmLocation farmerBiz,
   farmerToTruckDriver1 truckLabel binLabels startTime+3 tz
      farmLocation farmerBiz ,
   truckDriver1ToPackingHouse truckLabel binLabels startTime+4
            tz packingHouseLocation packingHouseBiz,
   applyFungicide binLabels startTime+5 tz
            packingHouseLocation packingHouseBiz ,
   sortingBoxing boxLabel binLabels startTime+6 tz
     packingHouseLocation packingHouseBiz,
   palletisation palletLabel boxLabels startTime+7 tz
      packingHouseLocation packingHouseBiz,
   packingHouseToTruckDriver2 truck2Label palletLabels startTime+8 tz
      packingHouseLocation packingHouseBiz,
   truckDriver2ToPortsOperator1 truck2Label palletLabels startTime+9 tz
      auPortLocation truck2biz,
   quarantineAus palletLabels startTime+10 tz
     auPortLocation regulator3biz,
   shippingToChina shipLabel palletLabels startTime+11 tz
     cnPortLocation cnPortBiz,
   quarantineChina palletLabels startTime+12 tz
      cnPortLocation regulator4biz
  ]



-- A series of events in a citrus supply chain.

--pest control
pestControl :: LabelEPC
            -> EPCISTime
            -> TimeZone
            -> ReadPointLocation
            -> BizLocation
            -> Event
pestControl blockId t tz location bizLocation =
  Event ObjectEventT Nothing
          (ObjectDWhat Observe blockId)
          (DWhen t Nothing tz)
          (DWhy Inspecting SellableNotAccessible)
          (DWhere [location] [bizLocation] [] [])

--check maximum residue of pesticides/fungicides
maxResidue :: LabelEPC
           -> EPCISTime
           -> TimeZone
           -> ReadPointLocation
           -> BizLocation
           -> Event
maxResidue blockId t tz location bizLocation =
  Event ObjectEventT Nothing
      (ObjectDWhat Observe blockId)
      (DWhen t Nothing tz)
      (DWhy Inspecting SellableNotAccessible)
      (DWhere [location] [bizLocation] [] [])

--label bins/harvest
labelBinsHarvest :: LabelEPC
                 -> EPCISTime
                 -> TimeZone
                 -> ReadPointLocation
                 -> BizLocation
                 -> Event
labelBinsHarvest binID t tz location bizLocation =
  Event ObjectEventT Nothing
      (ObjectDWhat Add binId) -- is Add the right action here?
      (DWhen t Nothing tz)
      (DWhy Commissioning Active)
      (DWhere [location] [bizLocation] [] [])

{- is this needed, or do we just make a transaction event with the parent
    being the truckID?
loadingTruckToPackingHouse :: [LabelEPC] -> LabelEPC -> EPCISTime -> TimeZone ->
  ReadPointLocation -> BizLocation -> Event
loadingTruckToPackingHouse binIds truckId t tz location bizLocation =
  Event AggregationEventT
  (AggregationDWhat Add truckId binIds)
  (DWhen t Nothing tz)
  (DWhy Loading SellableNotAccessible)
  (DWhere [location] [bizLocation] [] [])
  -}

--Transport
farmerToTruckDriver1 :: LabelEPC
                     -> [LabelEPC]
                     -> EPCISTime
                     -> TimeZone
                     -> ReadPointLocation
                     -> BizLocation
                     -> Event
farmerToTruckDriver1 truckId binIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Add truckId [] binIds)
  (DWhen t Nothing tz)
  (DWhy Loading InTransit)
  (DWhere [location] [bizLocation] [] [])

--Scan bins at packing house
truckDriver1ToPackingHouse :: LabelEPC
                           -> [LabelEPC]
                           -> EPCISTime
                           -> TimeZone
                           -> ReadPointLocation
                           -> BizLocation
                           -> Event
truckDriver1ToPackingHouse truckId binIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Delete truckId [] binIds)
  (DWhen t Nothing tz)
  (DWhy Accepting InProgress)
  (DWhere [location] [bizLocation] [] [])

--apply fungicide within 36 hours
applyFungicide :: [LabelEPC]
               -> EPCISTime
               -> TimeZone
               -> ReadPointLocation
               -> BizLocation
               -> Event
applyFungicide binIds t tz location bizLocation =
  Event TransformationT
  (TransformationDWhat Nothing binIds binIds)
  (DWhen t Nothing tz)
  (DWhy Inspecting SellableNotAccessible)
  (DWhere [location] [bizLocation] [] [])

--sorting and boxing
sortingBoxing :: LabelEPC
              -> [LabelEPC]
              -> EPCISTime
              -> TimeZone
              -> ReadPointLocation
              -> BizLocation
              -> Event
sortingBoxing boxId contents t tz location bizLocation =
  Event AggregationEventT
  (AggregationDWhat Add boxID contents)
  (DWhen t Nothing tz)
  (DWhy Commissioning Active)
  (DWhere [location] [bizLocation] [] [])

-- palletisation
palletisation :: LabelEPC
              -> [LabelEPC]
              -> EPCISTime
              -> TimeZone
              -> ReadPointLocation
              -> BizLocation
              -> Event
palletisation palletId boxes t tz location bizLocation =
  Event AggregationEventT
  (AggregationDWhat Add palletId boxes)
  (DWhen t Nothing tz)
  (DWhy Commissioning Active)
  (DWhere [location] [bizLocation] [] [])


--loading onto truck
packingHouseToTruckDriver2 :: LabelEPC
                           -> [LabelEPC]
                           -> EPCISTime
                           -> TimeZone
                           -> ReadPointLocation
                           -> BizLocation
                           -> Event
packingHouseToTruckDriver2 truckId palletIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Add truckId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy Loading InTransit)
  (DWhere [location] [bizLocation] [] [])

-- arrival of goods at the port
-- take them out of the truck
truckDriver2ToPortsOperator1 :: LabelEPC
                             -> [LabelEPC]
                             -> EPCISTime
                             -> TimeZone
                             -> ReadPointLocation
                             -> BizLocation
                             -> Event
truckDriver2ToPortsOperator1 truckId palletIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Remove truckId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy Loading InTransit)
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
  Event TransformationT
  (TransformationDWhat Nothing palletIds palletIds)
  (DWhen t Nothing tz)
  (DWhy Holding SellableNotAccessible)
  (DWhere [location] [bizLocation] [] [])

-- shipping to China
shippingToChina :: LabelEPC
                -> [LabelEPC]
                -> EPCISTime
                -> TimeZone
                -> ReadPointLocation
                -> BizLocation
                -> Event
shippingToChina shipId palletIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Remove shipId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy Shipping InTransit)
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
  Event TransformationT
  (TransformationDWhat Nothing palletIds palletIds)
  (DWhen t Nothing tz)
  (DWhy Holding SellableNotAccessible)
  (DWhere [location] [bizLocation] [] [])

