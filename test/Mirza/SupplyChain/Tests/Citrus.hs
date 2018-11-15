{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Tests.Citrus where

{-

Tests that should be implemented here

Check Provenance of a labelEPC
where I've used head, you need to use map to actually do it for all elements in the list. I've just done one element for illustrative purposes.
eventList ← listEvents <labelEPC>
let event = head eventList
eventInfo ← eventInfo(eventID)
(sig, uid) = head (signatures eventInfo)
publicKey ← getPublicKey uid
assert $ decrypt(sig, publicKey) == (joseText eventInfo)


Get all events that relate to a labelEPC
eventList ← listEvents <labelEPC>
subEvents eventList = [e | e ← eventList, if
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

      step "insert the users into SCS"
      userIdsSCS <- scsUsers

      step "insert the users into BR"
      userIdsBR <- brUsers

      step "insert citrus events into SCS, sign & counter sign them"

      step "check eventInfo for each event"

      step "get all events related to boxLabel"




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


-- All the labels that feed into citrusEvents
landLabel = GRAI farmerGS1CompanyPrefix "blockLabel" (SerialNumber "88")
binLabel = GIAI farmerGS1CompanyPrefix "binLabel" (SerialNumber "1")
truckLabel = SSCC truckDriver1GS1CompanyPrefix (SerialNumber "1")
binLabels = [binLabel, (GIAI farmerGS1CompanyPrefix "binLabel" (SerialNumber "2")]
boxLabel = GIAI farmerGS1CompanyPrefix "boxLabel" (SerialNumber "1")
palletLabel = GRAI packingHouseGS1CompanyPrefix "palletLabel" (SerialNumber "1")
boxLabels = [boxLabel, (GIAI farmerGS1CompanyPrefix "boxLabel" (SerialNumber "2")]
palletLabels = [palletLabel, (GRAI packingHouseGS1CompanyPrefix "palletLabel" (SerialNumber "2")]
truck2Label = SSCC truckDriver2GS1CompanyPrefix (SerialNumber "1")



-- Create a list of events starting at "startTime" in a particular
-- timezone.
citrusEvents :: EPCISTime -> Timezone -> [Event]
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


-- Create users in the SCS db. Need to also create them in
-- the BR. This should be re-implemented as a client fucntion, so
-- you can do it the same way in both SCS and BR.
scsUsers :: [ClientM UserId]
scsUsers = do
  let userNames = ["regulator1", "regulator2", "farmer", "truckDriver1",
  "packingHouseOperator", "truckDriver2", "portsOperator1",
  "shippingCompany", "regulator3", "regulator4"]
  let nUsers = length userNames
  let initPrefix = 11111111
  let gs1companyPrefixes = map show [initPrefix.. initPrefix+nUsers]
  pure $ insertMultipleUsersSCS
    "citrusSupplyChain" userNames gs1companyPrefixes




-- A series of events in a citrus supply chain.

--pest control
pestControl :: LabelEPC -> EPCISTime -> Timezone ->
  ReadPointLocation -> BizLocation ->  Event
pestControl blockId t tz location bizLocation =
  Event ObjectEventT Nothing
          (ObjectDWhat Observe blockId)
          (DWhen t Nothing tz)
          (DWhy Inspecting SellableNotAccessible)
          (DWhere [location] [bizLocation] [] [])

--check maximum residue of pesticides/fungicides
maxResidue :: LabelEPC -> EPCISTime -> Timezone ->
  ReadPointLocation -> BizLocation ->  Event
maxResidue blockId t tz location bizLocation =
  Event ObjectEventT Nothing
      (ObjectDWhat Observe blockId)
      (DWhen t Nothing tz)
      (DWhy Inspecting SellableNotAccessible)
      (DWhere [location] [bizLocation] [] [])

--label bins/harvest
labelBinsHarvest :: LabelEPC -> EPCISTime -> Timezone ->
  ReadPointLocation -> BizLocation ->  Event
labelBinsHarvest binID t tz location bizLocation =
  Event ObjectEventT Nothing
      (ObjectDWhat Add binId) -- is Add the right action here?
      (DWhen t Nothing tz)
      (DWhy Commissioning Active)
      (DWhere [location] [bizLocation] [] [])

{- is this needed, or do we just make a transaction event with the parent
    being the truckID?
loadingTruckToPackingHouse :: [LabelEPC] -> LabelEPC -> EPCISTime -> Timezone ->
  ReadPointLocation -> BizLocation -> Event
loadingTruckToPackingHouse binIds truckId t tz location bizLocation =
  Event AggregationEventT
  (AggregationDWhat Add truckId binIds)
  (DWhen t Nothing tz)
  (DWhy Loading SellableNotAccessible)
  (DWhere [location] [bizLocation] [] [])
  -}

--Transport
farmerToTruckDriver1 :: LabelEPC -> [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
farmerToTruckDriver1 truckId binIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Add truckId [] binIds)
  (DWhen t Nothing tz)
  (DWhy Loading InTransit)
  (DWhere [location] [bizLocation] [] [])

--Scan bins at packing house
truckDriver1ToPackingHouse :: LabelEPC -> [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
truckDriver1ToPackingHouse truckId binIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Delete truckId [] binIds)
  (DWhen t Nothing tz)
  (DWhy Accepting InProgress)
  (DWhere [location] [bizLocation] [] [])

--apply fungicide within 36 hours
applyFungicide ::  [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
applyFungicide binIds t tz location bizLocation =
  Event TransformationT
  (TransformationDWhat Nothing binIds binIds)
  (DWhen t Nothing tz)
  (DWhy Inspecting SellableNotAccessible)
  (DWhere [location] [bizLocation] [] [])

--sorting and boxing
sortingBoxing :: LabelEPC -> [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
sortingBoxing boxId contents t tz location bizLocation =
  Event AggregationEventT
  (AggregationDWhat Add boxID contents)
  (DWhen t Nothing tz)
  (DWhy Commissioning Active)
  (DWhere [location] [bizLocation] [] [])

-- palletisation
palletisation :: LabelEPC -> [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
palletisation palletId boxes t tz location bizLocation =
  Event AggregationEventT
  (AggregationDWhat Add palletId boxes)
  (DWhen t Nothing tz)
  (DWhy Commissioning Active)
  (DWhere [location] [bizLocation] [] [])



--loading onto truck
packingHouseToTruckDriver2 :: LabelEPC -> [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
packingHouseToTruckDriver2 truckId palletIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Add truckId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy Loading InTransit)
  (DWhere [location] [bizLocation] [] [])

-- arrival of goods at the port
-- take them out of the truck
truckDriver2ToPortsOperator1 :: LabelEPC -> [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
truckDriver2ToPortsOperator1 truckId palletIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Remove truckId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy Loading InTransit)
  (DWhere [location] [bizLocation] [] [])

-- quarantine in australia
-- transformed state from non-quarantined to quarantined
quarantineAus ::  [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
quarantineAus palletIds t tz location bizLocation =
  Event TransformationT
  (TransformationDWhat Nothing palletIds palletIds)
  (DWhen t Nothing tz)
  (DWhy Holding SellableNotAccessible)
  (DWhere [location] [bizLocation] [] [])

-- shipping to China
shippingToChina :: LabelEPC -> [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
shippingToChina shipId palletIds t tz location bizLocation =
  Event TransactionEventT
  (TransactionDWhat Remove shipId [] palletIds)
  (DWhen t Nothing tz)
  (DWhy Shipping InTransit)
  (DWhere [location] [bizLocation] [] [])

-- quarantine in China
-- transformed state from non-quarantined to quarantined
quarantineChina ::  [LabelEPC] -> EPCISTime -> Timezone
  ReadPointLocation -> BizLocation -> Event
quarantineChina palletIds t tz location bizLocation =
  Event TransformationT
  (TransformationDWhat Nothing palletIds palletIds)
  (DWhen t Nothing tz)
  (DWhy Holding SellableNotAccessible)
  (DWhere [location] [bizLocation] [] [])





