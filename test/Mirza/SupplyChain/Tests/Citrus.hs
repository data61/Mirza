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
Keys
add, get, getInfo public key
revoke public key
..these will be moved into the registery soon.
Contacts
Add, remove and search for contacts.


-}

-- Need to first create the users for these events (see citrusEntities
-- below), and instantitate them with appropriate labelEPCs.
citrusEvents :: [Event]
citrusEvents =
  [pestControl,
   maxResidue,
   labelBinsHarvest,
   farmerToTruckDriver1,
   truckDriver1ToPackingHouse,
   applyFungicide,
   sortingBoxing,
   palletisation,
   packingHouseToTruckDriver2,
   truckDriver2ToPortsOperator1,
   quarantineAus,
   shippingToChina,
   quarantineChina
  ]


-- Create users in the SCS db. Need to also create them in
-- the BR. This should be re-implemented as a client fucntion, so
-- you can do it the same way in both SCS and BR.
scsUsers :: [AppM context err UserId]
scsUsers = do
  let userNames = ["regulator1", "regulator2", "farmer", "truckDriver1",
  "packingHouseOperator", "truckDriver2", "portsOperator1",
  "shippingCompany", "regulator3"]
  let nUsers = length userNames
  let initPrefix = 11111111
  let gs1companyPrefixes = map show [initPrefix.. initPrefix+nUsers]
  pure $ insertMultipleUsersSCS
    "citrusSupplyChain" userNames gs1companyPrefixes


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
sortingBoxing contents t tz location bizLocation =
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





