{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Tests.Citrus where

import           Control.Exception                      (bracket)

import qualified Data.Text                              as T

import           Mirza.BusinessRegistry.Client.Servant  as BRClient

import           Mirza.BusinessRegistry.Tests.Generate
import           Mirza.SupplyChain.Tests.Generate

import           Mirza.Common.Tests.InitClient
import           Mirza.Common.Tests.ServantUtils        (runClient)

import           Servant.Client                         (ClientM)

import           Servant.API.BasicAuth                  (BasicAuthData (..))

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Mirza.BusinessRegistry.Database.Schema (LocationId)
import           Mirza.BusinessRegistry.Types           as BT

import           Mirza.SupplyChain.Types                as ST

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC
import           Data.GS1.Event

import           Data.Time                              (TimeZone, addUTCTime)

import           Crypto.JOSE.JWK                        (JWK)
import           Mirza.BusinessRegistry.Tests.Utils     (readJWK)

import           Data.Maybe                             (fromJust)

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
        ht <- H.new
        ht <- insertAndAuth ht allEntities
        fmap (insertEachEvent ht) citrusEvents

        {-
          step "Sanity check: That all the fake lists are in order"
          length allPrefixes `shouldBe` length allLocationEPC
          length allPrefixes `shouldBe` length locationList
          length allPrefixes `shouldBe` length userNames
          length allPrefixes `shouldBe` length businessList

          step "insert prelim data into SCS and BR"
          _userIdsSCS <- httpSCS scsUsers
          _gs1prefixes <- httpBR $ insertBusinesses brAuthUser businessList
          _locationIds <- httpBR $ insertLocations brAuthUser locationList

          step "insert the users into BR"
          _userIdsBR <- httpBR $ brUsers brAuthUser

          step "insert citrus events into SCS, sign & counter sign them"
          -- for each event in CitrusEvents,
          -- insert key(s) into BR
          -- insert event into SCS
          -- sign (and countersign) using the keys you inserted, and create
          -- a SignedEvent. insert into SCS using eventSign.

          step "check eventInfo for each event"

          step "get all events related to boxLabel"
          error "not implemented yet"
          -}

  pure $ testGroup "Citrus Client tests"
        [ citrusSupplyChainTests
        ]

type AuthHash = HashTable Entity (UserId, BasicAuthData, KeyId)



insertAndAuth :: [Entity] -> AuthHash -> ClientM AuthHash
insertAndAuth
  (Entity name gs1companyPrefix bizName locations (KeyPair privateKey publicKey)):entities
  ht = do
  --make NewUser
  --userId <- insert NewUser into SCS
  --basicAuthData <- login
  --add Business to BR (using gs1companyPrefix and bizName)
  --make BR New User
  --brUserId <- insert New User into BR
  --basicAuthDataBr <- login userId br
  -- XXX only need to do addLocation here if the location is tied to the user in the BR,
  -- otherwise it's probably easier to just add all the locations separately.
  --addLocation basicAuthDataBr newLocation
  --keyId <- addPublicKey basicAuthDataBr publicKey
  --H.insert ht (userId, basicAuthData, keyId)


insertEachEvent :: EachEvent -> AuthHash ->  ClientM ()
insertEachEvent ht (EachEvent [] ev) = pure ()
insertEachEvent ht (EachEvent initialEntity: entities ev) = do
  let (userId, auth) = lookup ht initialEntity
  -- first insert it
  (eventInfo, eventId) = case (EventType ev) of
                           -- I know using fromJust is bad here, but will fix later.
                           AggregationEventT -> httpSCS insertAggEvent (fromJust $ mkAggEvent ev)
                           ObjectEventT -> httpSCS insertObjectEvent (fromJust $ mkObjectEvent ev)
                           TransactionEventT -> httpSCS insertTransactEvent (fromJust $ mkTransactEvent ev)
                           TransformationEventT -> httpSCS insertTransfEvent (fromJust $ mkTransfEvent ev)

  fmap clientSignEvent entities




clientSignEvent :: Entity -> HashTable Entity (UserId, BasicAuthData) -> EventInfo -> IO ()
clientSignEvent entity ht eventInfo = do
  let (userId, auth) = lookup ht initialEntity
  let (Entity name gs1companyPrefix bizName locations (KeyPair privateKey publicKey)) = initialEntity
  --then sign it
  let signedEvent = SignedEvent .... (eventToSign eventInfo) --TODO: sign this with the private key
  _ <- httpSCS eventSign auth signedEvent


data EachEvent = EachEvent [Entity] Event

data Entity = Entity EntityName GS1CompanyPrefix BusinessName [LocationEPC] KeyPair

type EntityName = T.Text
type BusinessName = T.Text

type HashTable k v = H.BasicHashTable k v

-- | A series of events in a citrus supply chain.
citrusEvents :: EPCISTime -> TimeZone -> [EachEvent]
citrusEvents startTime tz =
  [
    EachEvent [regulator1E]
    (pestControl [instanceLandLabel]
    startTime tz
    rpFarmLocation (BizLocation regulator1Biz)),


    EachEvent [regulator2E]
    (maxResidue [instanceLandLabel]
    (addEpcisTime startTime 1) tz
    rpFarmLocation (BizLocation regulator2Biz)),

    EachEvent [farmerE]
    (labelBinsHarvest [instanceLandLabel]
    (addEpcisTime startTime 2) tz
    rpFarmLocation (BizLocation farmerBiz)),

    EachEvent [farmerE, truckDriver1E]
    (farmerToTruckDriver1
    parentTruckLabel binLabels
    (addEpcisTime startTime 3) tz
    rpFarmLocation (BizLocation farmerBiz)),

    EachEvent [truckDriver1E, packingHouseE]
    (truckDriver1ToPackingHouse parentTruckLabel binLabels
    (addEpcisTime startTime 4) tz
    rpPackingHouseLocation locationPackingHouse),

    EachEvent [packingHouseE]
    (applyFungicide binLabels
    (addEpcisTime startTime 5) tz
    rpPackingHouseLocation locationPackingHouse),

    EachEvent [packingHouseE]
    (sortingBoxing (Just . ParentLabel $ boxLabel) binLabels
    (addEpcisTime startTime 6) tz
    rpPackingHouseLocation locationPackingHouse),

    EachEvent [packingHouseE]
    (palletisation (Just . ParentLabel $ palletLabel)  boxLabels
    (addEpcisTime startTime 7) tz
    rpPackingHouseLocation locationPackingHouse),

    EachEvent [packingHouseE, truckDriver2E]
    (packingHouseToTruckDriver2 parentTruck2Label palletLabels
    (addEpcisTime startTime 8) tz
    rpPackingHouseLocation locationPackingHouse),

    EachEvent [truckDriver2E, auPortE]
    (truckDriver2ToPortsOperator1 parentTruck2Label palletLabels
    (addEpcisTime startTime 9) tz
    rpAuPort (BizLocation truck2Biz)),

    EachEvent [regulator3E]
    (quarantineAus palletLabels
    (addEpcisTime startTime 10) tz
    rpAuPort (BizLocation regulator3Biz)),

    EachEvent [auPortE, cnPortE]
    (shippingToChina (Just . ParentLabel $ shipLabel) palletLabels
    (addEpcisTime startTime 11) tz
    rpCnPort (BizLocation cnPortLocation)),

    EachEvent [regulator4E]
    (quarantineChina palletLabels
    (addEpcisTime startTime 12) tz
    rpCnPort (BizLocation regulator4Biz))

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


-- Entities
farmerE = Entity "farmer" farmerCompanyPrefix "Citrus Sensation Farm"
  [farmLocation, farmerBiz] farmerKP

truckDriver1E = Entity "truckDriver1" truckDriver1CompanyPrefix "Super Transport Solutions"
  [truckDriver1Biz] truckDriver1KP

regulator1E = Entity "regulator1" regulator1CompanyPrefix "Pest Controllers"
  [regulator1Biz] regulator1KP

regulator2E = Entity "regulator2" regulator2CompanyPrefix "Residue Checkers"
  [regulator2Biz] regulator2KP

packingHouseE = Entity "packingHouse" packingHouseCompanyPrefix "Packing Citrus R Us"
  [packingHouseLocation] packingHouseKP

auPortE = Entity "AustralianPort" auPortCompanyPrefix "Port Melbourne"
  [auPortLocation] auPortKP

cnPortE = Entity "ChinesePort" cnPortCompanyPrefix "Shanghai Port"
  [cnPortLocation] cnPortKP

truckDriver2E = Entity "truckDriver2" truck2CompanyPrefix "Duper Transport Solutions"
  [truck2Biz] truck2KP

regulator3E = Entity "regulator3" regulator3CompanyPrefix "Quarantine Australia"
  [regulator3Biz] regulator3KP

regulator4E = Entity "regulator4" regulator4CompanyPrefix "Quarantine China"
  [regulator4Biz] regulator4KP



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
truckDriver1Biz :: LocationEPC
truckDriver1Biz = SGLN truckDriver1CompanyPrefix (LocationReference "1") Nothing
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
  , truckDriver1Biz
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
  , NewLocation truckDriver1Biz (Just (Latitude 130.7, Longitude 213.9)) (Just "50 Bridge Street, Surry Hills")
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

insertBusinesses :: BasicAuthData -> [NewBusiness] -> ClientM [GS1CompanyPrefix]
insertBusinesses brAuthUser bizList = sequence $ BRClient.addBusiness brAuthUser <$> bizList

insertLocations :: BasicAuthData -> [NewLocation] -> ClientM [LocationId]
insertLocations brAuthUser locs = sequence $ BRClient.addLocation brAuthUser <$> locs


--TODO: Create enough key pairs for all the supply chain entities
--and save them in the TestData/Citrus dir
type PrivateKey = JWK
type PublicKey = JWK
data KeyPair = KeyPair (IO PrivateKey) (IO PublicKey)

privateKeyPath :: String -> FilePath
privateKeyPath entityName = "./test/Mirza/SupplyChain/TestData/testKeys/goodJWKs/" ++ entityName ++ "_rsa.json"

publicKeyPath :: String -> FilePath
publicKeyPath entityName = "./test/Mirza/SupplyChain/TestData/testKeys/goodJWKs/" ++ entityName ++ "_rsa_pub.json"


farmerKP = KeyPair (fromJust <$> readJWK $ privateKeyPath "farmer") (readJWK $ publicKeyPath "farmer")
truckDriver1KP = KeyPair (readJWK $ privateKeyPath "truckDriver1") (readJWK $ publicKeyPath "truckDriver1")
regulator1KP = KeyPair (readJWK $ privateKeyPath "regulator1") (readJWK $ publicKeyPath "regulator1")
regulator2KP = KeyPair (readJWK $ privateKeyPath "regulator2") (readJWK $ publicKeyPath "regulator2")
packingHouseKP = KeyPair (readJWK $ privateKeyPath "packingHouse") (readJWK $ publicKeyPath "packingHouse")
auPortKP = KeyPair (readJWK $ privateKeyPath "auPort") (readJWK $ publicKeyPath "auPort")
cnPortKP = KeyPair (readJWK $ privateKeyPath "cnPort") (readJWK $ publicKeyPath "cnPort")
truck2KP = KeyPair (readJWK $ privateKeyPath "truck2") (readJWK $ publicKeyPath "truck2")
regulator3KP = KeyPair (readJWK $ privateKeyPath "regulator3") (readJWK $ publicKeyPath "regulator3")
regulator4KP = KeyPair (readJWK $ privateKeyPath "regulator4") (readJWK $ publicKeyPath "regulator4")
-- ...etc.




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

userNames :: [T.Text]
userNames = [
    "farmer"
  , "truckDriver1"
  , "regulator1"
  , "regulator2"
  , "packingHouse"
  , "auPort"
  , "cnPort"
  , "truck2"
  , "regulator3"
  , "regulator4"
  ]


-- Create users in the SCS db. Need to also create them in
-- the BR. This should be re-implemented as a client fucntion, so
-- you can do it the same way in both SCS and BR.
scsUsers :: ClientM [ST.UserId]
scsUsers = insertMultipleUsersSCS "citrusSupplyChain" userNames allPrefixes


brUsers :: BasicAuthData -> ClientM [BT.UserId]
brUsers brAuthUser =
  insertMultipleUsersBR "citrusSupplyChain" brAuthUser userNames allPrefixes



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
