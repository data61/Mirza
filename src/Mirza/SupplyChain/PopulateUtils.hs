{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.PopulateUtils where

import           GHC.Generics                          (Generic)

import           Mirza.Common.Utils

import           Control.Monad.Except
import           Control.Monad.Identity

import           Data.Foldable                         (traverse_)

import           Data.GS1.EPC
import           Data.GS1.Event
import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy

import           Data.Time                             (TimeZone, addUTCTime, getCurrentTime)
import           Data.Time.LocalTime                   (utc)

import qualified Mirza.BusinessRegistry.Types          as BT
import           Mirza.SupplyChain.Types               as ST

import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)

import           Servant.API.BasicAuth                 (BasicAuthData (..))

import           Text.Email.Validate                   (toByteString)

import           Data.Hashable                         (Hashable (..))
import           Data.HashMap.Lazy                     as H

import           Servant.Client                        (BaseUrl (..), ClientM, Scheme (..))

import           Mirza.BusinessRegistry.Client.Servant as BRClient
import           Mirza.SupplyChain.Client.Servant      as SCSClient

import           Crypto.JOSE                           (Alg (RS256),
                                                        newJWSHeader, signJWS)
import qualified Crypto.JOSE                           as JOSE
import           Crypto.JOSE.Types                     (Base64Octets (..))

import           Mirza.BusinessRegistry.GenerateUtils  (genMultipleUsersBR)
import           Mirza.SupplyChain.GenerateUtils

import           Data.Maybe                            (fromJust)

import           Data.List.NonEmpty                    (NonEmpty (..))


-- =============================================================================
-- Utility Data structures/Type aliases
-- =============================================================================

type EntityName = T.Text
type BusinessName = T.Text

data EachEvent = EachEvent {
    eachEventEntities :: [Entity]
  , eachEventEvent    :: Event
  }
  deriving (Eq, Show, Generic)

data Entity = Entity {
    entitiName          :: EntityName
  , entityCompanyPrefix :: GS1CompanyPrefix
  , entityBizName       :: BusinessName
  , entityLocList       :: [LocationEPC]
  , entityKeyPairPaths  :: KeyPairPaths
  }
  deriving (Eq, Show, Generic)
instance Hashable Entity where
  hashWithSalt salt entity = hashWithSalt salt (unGS1CompanyPrefix $ entityCompanyPrefix entity)

--                              PrivateKey PublicKey
data KeyPairPaths = KeyPairPaths FilePath FilePath
  deriving (Eq, Show, Generic)

privateKeyPath :: String -> FilePath
privateKeyPath entityName = "./test/Mirza/SupplyChain/TestData/testKeys/goodJWKs/" <> entityName <> "_rsa.json"

publicKeyPath :: String -> FilePath
publicKeyPath entityName = "./test/Mirza/SupplyChain/TestData/testKeys/goodJWKs/" <> entityName <> "_rsa_pub.json"

type AuthHash = H.HashMap Entity (UserId, BasicAuthData, BRKeyId)
type LocationMap = H.HashMap LocationEPC BT.NewLocation


-- =============================================================================
-- Insertion and Signature utils
-- =============================================================================

insertCitrusData :: BaseUrl -> BaseUrl -> BasicAuthData -> IO [Event]
insertCitrusData scsUrl brUrl brAuthUser = do
  let httpSCS = runClient scsUrl
      initAuthHt = H.empty
  authHt <- insertAndAuth scsUrl brUrl brAuthUser locationMap initAuthHt allEntities
  currTime <- getCurrentTime
  let citrusEachEvents = makeCitrusEvents (EPCISTime currTime) utc
      citrusEvents = eachEventEvent <$> citrusEachEvents
  _insertionRes <- traverse (httpSCS . (insertEachEvent authHt)) citrusEachEvents
  pure citrusEvents

insertAndAuth :: BaseUrl
              -> BaseUrl
              -> BasicAuthData
              -> LocationMap
              -> AuthHash
              -> [Entity]
              -> IO AuthHash
insertAndAuth _ _ _ _          ht [] = pure ht
insertAndAuth scsUrl brUrl auth locMap ht (entity:entities) = do
  let httpSCS = runClient scsUrl
      httpBR = runClient brUrl
      (Entity name companyPrefix bizName locations (KeyPairPaths _ pubKeyPath)) = entity
      [newUserSCS] = genMultipleUsersSCS "citrusTest" 1 [name] [companyPrefix]
      [newUserBR] = genMultipleUsersBR "citrusTest" 1 [name] [companyPrefix]
      newBiz = BT.NewBusiness companyPrefix bizName
  pubKey <- fmap expectJust $ liftIO $ readJWK pubKeyPath
  insertedUserIdSCS <- fmap expectRight $ httpSCS $ SCSClient.addUser newUserSCS
  _insertedUserIdBR <- httpBR $ BRClient.addUser auth newUserBR
  _insertedPrefix <- httpBR $ BRClient.addBusiness auth newBiz
  brKeyId <- fmap expectRight $ httpBR $ BRClient.addPublicKey auth pubKey Nothing
  let basicAuthDataSCS =
        BasicAuthData
          (toByteString . ST.newUserEmailAddress $ newUserSCS)
          (encodeUtf8   . ST.newUserPassword     $ newUserSCS)

  let newLocs = flip H.lookup locMap <$> locations
  traverse_  maybeInsertLocation newLocs
  let updatedHt = H.insert entity (insertedUserIdSCS, basicAuthDataSCS, brKeyId) ht
  insertAndAuth scsUrl brUrl auth locMap updatedHt entities
  where
    maybeInsertLocation Nothing    = pure ()
    maybeInsertLocation (Just loc) = void $ runClient brUrl $ BRClient.addLocation auth loc

insertEachEvent :: AuthHash -> EachEvent ->  ClientM ()
insertEachEvent _ (EachEvent [] _) = pure ()
insertEachEvent ht (EachEvent (initialEntity: entities) ev) = do
  let (entityUserId, auth, _) = expectJust $ H.lookup initialEntity ht
  (insertedEventInfo, _eventId) <- case _etype ev of
          AggregationEventT -> SCSClient.insertAggEvent auth (fromJust $ mkAggEvent ev)
          ObjectEventT -> SCSClient.insertObjectEvent auth (fromJust $ mkObjectEvent ev)
          TransactionEventT -> SCSClient.insertTransactEvent auth (fromJust $ mkTransactEvent ev (entityUserId :| []))
          TransformationEventT -> SCSClient.insertTransfEvent auth (fromJust $ mkTransfEvent ev)

  traverse_ (clientSignEvent ht insertedEventInfo) entities


clientSignEvent :: AuthHash -> EventInfo -> Entity -> ClientM EventInfo
clientSignEvent ht evInfo entity = do
  let (_, auth, _) = expectJust $ H.lookup entity ht
      (EventInfo event _ _ (Base64Octets toSign) _) = evInfo
      eventId = fromJust $ _eid event
      (Entity _ _ _ _ (KeyPairPaths privKeyPath pubKeyPath)) = entity
  privKey <- fmap expectJust $ liftIO $ readJWK privKeyPath
  pubKey <- fmap expectJust $ liftIO $ readJWK pubKeyPath
  keyId <- BRClient.addPublicKey auth pubKey Nothing

  s <- liftIO $ runExceptT @JOSE.Error (
          signJWS toSign (Identity (newJWSHeader ((), RS256), privKey))
          )
  eventSign auth . SignedEvent eventId keyId . expectRight $ s



-- =============================================================================
-- Event list/data - Dummy Data of a Citrus supply chain
-- =============================================================================

-- | A series of events in a citrus supply chain.
makeCitrusEvents :: EPCISTime -> TimeZone -> [EachEvent]
makeCitrusEvents startTime tz =
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
    (palletisation (Just . ParentLabel $ palletLabel) boxLabels
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
farmerE :: Entity
farmerE = Entity "farmer" farmerCompanyPrefix "Citrus Sensation Farm" [farmLocation, farmerBiz] farmerKP
truckDriver1E :: Entity
truckDriver1E = Entity "truckDriver1" truckDriver1CompanyPrefix "Super Transport Solutions" [truckDriver1Biz] truckDriver1KP
regulator1E :: Entity
regulator1E = Entity "regulator1" regulator1CompanyPrefix "Pest Controllers" [regulator1Biz] regulator1KP
regulator2E :: Entity
regulator2E = Entity "regulator2" regulator2CompanyPrefix "Residue Checkers" [regulator2Biz] regulator2KP
packingHouseE :: Entity
packingHouseE = Entity "packingHouse" packingHouseCompanyPrefix "Packing Citrus R Us" [packingHouseLocation] packingHouseKP
auPortE :: Entity
auPortE = Entity "AustralianPort" auPortCompanyPrefix "Port Melbourne" [auPortLocation] auPortKP
cnPortE :: Entity
cnPortE = Entity "ChinesePort" cnPortCompanyPrefix "Shanghai Port" [cnPortLocation] cnPortKP
truckDriver2E :: Entity
truckDriver2E = Entity "truckDriver2" truck2CompanyPrefix "Duper Transport Solutions" [truck2Biz] truck2KP
regulator3E :: Entity
regulator3E = Entity "regulator3" regulator3CompanyPrefix "Quarantine Australia" [regulator3Biz] regulator3KP
regulator4E :: Entity
regulator4E = Entity "regulator4" regulator4CompanyPrefix "Quarantine China" [regulator4Biz] regulator4KP


allEntities :: [Entity]
allEntities = [
    farmerE
  , truckDriver1E
  , regulator1E
  , regulator2E
  , packingHouseE
  , auPortE
  , cnPortE
  , truckDriver2E
  , regulator3E
  , regulator4E
  ]


farmerCompanyPrefix :: GS1CompanyPrefix
farmerCompanyPrefix = GS1CompanyPrefix "1111111"
truckDriver1CompanyPrefix :: GS1CompanyPrefix
truckDriver1CompanyPrefix = GS1CompanyPrefix "2222111"
regulator1CompanyPrefix :: GS1CompanyPrefix
regulator1CompanyPrefix = GS1CompanyPrefix "3333111"
regulator2CompanyPrefix :: GS1CompanyPrefix
regulator2CompanyPrefix = GS1CompanyPrefix "4444111"
packingHouseCompanyPrefix :: GS1CompanyPrefix
packingHouseCompanyPrefix = GS1CompanyPrefix "5555111"
auPortCompanyPrefix :: GS1CompanyPrefix
auPortCompanyPrefix = GS1CompanyPrefix "7777111"
cnPortCompanyPrefix :: GS1CompanyPrefix
cnPortCompanyPrefix = GS1CompanyPrefix "8888111"
truck2CompanyPrefix :: GS1CompanyPrefix
truck2CompanyPrefix = GS1CompanyPrefix "1212111"
regulator3CompanyPrefix :: GS1CompanyPrefix
regulator3CompanyPrefix = GS1CompanyPrefix "4545111"
regulator4CompanyPrefix :: GS1CompanyPrefix
regulator4CompanyPrefix = GS1CompanyPrefix "8989111"


farmLocation :: LocationEPC
farmLocation = SGLN farmerCompanyPrefix (LocationReference "1") Nothing -- "blockID3"
truckDriver1Biz :: LocationEPC
truckDriver1Biz = SGLN truckDriver1CompanyPrefix (LocationReference "2") Nothing
regulator1Biz :: LocationEPC
regulator1Biz = SGLN regulator1CompanyPrefix (LocationReference "3") Nothing
regulator2Biz :: LocationEPC
regulator2Biz = SGLN regulator2CompanyPrefix (LocationReference "4") Nothing
packingHouseLocation :: LocationEPC
packingHouseLocation = SGLN packingHouseCompanyPrefix (LocationReference "5") Nothing
auPortLocation :: LocationEPC
auPortLocation = SGLN auPortCompanyPrefix (LocationReference "6") Nothing
cnPortLocation :: LocationEPC
cnPortLocation = SGLN cnPortCompanyPrefix (LocationReference "7") Nothing
farmerBiz :: LocationEPC
farmerBiz = SGLN farmerCompanyPrefix (LocationReference "8") Nothing
packingHouseBiz :: LocationEPC
packingHouseBiz = SGLN packingHouseCompanyPrefix (LocationReference "9") Nothing
truck2Biz :: LocationEPC
truck2Biz = SGLN truck2CompanyPrefix (LocationReference "10") Nothing
regulator3Biz :: LocationEPC
regulator3Biz = SGLN regulator3CompanyPrefix (LocationReference "11") Nothing
regulator4Biz :: LocationEPC
regulator4Biz = SGLN regulator4CompanyPrefix (LocationReference "12") Nothing


locationMap :: LocationMap
locationMap =
  let newHt = H.empty
      updatedHt = H.insert farmLocation (BT.NewLocation farmLocation (Just (BT.Latitude 122.3, BT.Longitude 123.9)) (Just "17 Cherry Drive, Young")) newHt
      updatedHt1 = H.insert truckDriver1Biz (BT.NewLocation truckDriver1Biz (Just (BT.Latitude 130.7, BT.Longitude 213.9)) (Just "50 Bridge Street, Surry Hills")) updatedHt
      updatedHt2 = H.insert regulator1Biz (BT.NewLocation regulator1Biz (Just (BT.Latitude 192.3, BT.Longitude 113.9)) (Just "NSW PestControl, Wyong")) updatedHt1
      updatedHt3 = H.insert regulator2Biz (BT.NewLocation regulator2Biz (Just (BT.Latitude 134.6, BT.Longitude 126.9)) (Just "7 Citrus Street, Gordon")) updatedHt2
      updatedHt4 = H.insert packingHouseLocation (BT.NewLocation packingHouseLocation (Just (BT.Latitude 102.3, BT.Longitude 110.9)) (Just "14 Plucking Street, WoyWoy")) updatedHt3
      updatedHt5 = H.insert auPortLocation (BT.NewLocation auPortLocation (Just (BT.Latitude 190.3, BT.Longitude 115.8)) (Just "21 Pitkin Avenue, Muswellbrook")) updatedHt4
      updatedHt6 = H.insert cnPortLocation (BT.NewLocation cnPortLocation (Just (BT.Latitude 234.3, BT.Longitude 137.8)) (Just "34 Park Boulevard, Merriwa")) updatedHt5
      updatedHt7 = H.insert farmerBiz (BT.NewLocation farmerBiz (Just (BT.Latitude 291.3, BT.Longitude 173.2)) (Just "23 Cleveland Street, Surry Hills")) updatedHt6
      updatedHt8 = H.insert packingHouseBiz (BT.NewLocation packingHouseBiz (Just (BT.Latitude 182.5, BT.Longitude 120.1)) (Just "141 Homer Street, Ashfield")) updatedHt7
      updatedHt9 = H.insert truck2Biz (BT.NewLocation truck2Biz (Just (BT.Latitude 222.2, BT.Longitude 112.1)) (Just "90 Crescent Road, Moss Vale")) updatedHt8
      updatedHt10 = H.insert regulator3Biz (BT.NewLocation regulator3Biz (Just (BT.Latitude 165.1, BT.Longitude 114.6)) (Just "37 York Street")) updatedHt9
      in
      H.insert regulator4Biz (BT.NewLocation regulator4Biz (Just (BT.Latitude 154.3, BT.Longitude 119.9)) (Just "63 Chopin Street, Woolloomolloo")) updatedHt10


farmerKP :: KeyPairPaths
farmerKP = KeyPairPaths (privateKeyPath "farmer") (publicKeyPath "farmer")
truckDriver1KP :: KeyPairPaths
truckDriver1KP = KeyPairPaths (privateKeyPath "truckDriver1") (publicKeyPath "truckDriver1")
regulator1KP :: KeyPairPaths
regulator1KP = KeyPairPaths (privateKeyPath "regulator1") (publicKeyPath "regulator1")
regulator2KP :: KeyPairPaths
regulator2KP = KeyPairPaths (privateKeyPath "regulator2") (publicKeyPath "regulator2")
packingHouseKP :: KeyPairPaths
packingHouseKP = KeyPairPaths (privateKeyPath "packingHouse") (publicKeyPath "packingHouse")
auPortKP :: KeyPairPaths
auPortKP = KeyPairPaths (privateKeyPath "auPort") (publicKeyPath "auPort")
cnPortKP :: KeyPairPaths
cnPortKP = KeyPairPaths (privateKeyPath "cnPort") (publicKeyPath "cnPort")
truck2KP :: KeyPairPaths
truck2KP = KeyPairPaths (privateKeyPath "truck2") (publicKeyPath "truck2")
regulator3KP :: KeyPairPaths
regulator3KP = KeyPairPaths (privateKeyPath "regulator3") (publicKeyPath "regulator3")
regulator4KP :: KeyPairPaths
regulator4KP = KeyPairPaths (privateKeyPath "regulator4") (publicKeyPath "regulator4")


-- All the labels that feed into Citrus Events
landLabel :: InstanceLabelEPC
landLabel = GRAI farmerCompanyPrefix (AssetType "blockLabel") (SerialNumber "1")
binLabel :: InstanceLabelEPC
binLabel = GIAI farmerCompanyPrefix (SerialNumber "3")
truckLabel :: InstanceLabelEPC
truckLabel = SSCC truckDriver1CompanyPrefix (SerialNumber "4")
boxLabel :: InstanceLabelEPC
boxLabel = GIAI farmerCompanyPrefix (SerialNumber "5")
palletLabel :: InstanceLabelEPC
palletLabel = GRAI packingHouseCompanyPrefix (AssetType "palletLabel") (SerialNumber "6")
truck2Label :: InstanceLabelEPC
truck2Label = SSCC truck2CompanyPrefix (SerialNumber "7")
shipLabel :: InstanceLabelEPC
shipLabel = SSCC cnPortCompanyPrefix (SerialNumber "8")
binLabels :: [LabelEPC]
binLabels = [IL binLabel, IL $ GIAI farmerCompanyPrefix (SerialNumber "9")]
boxLabels :: [LabelEPC]
boxLabels = [IL boxLabel, IL $ GIAI farmerCompanyPrefix (SerialNumber "10")]
palletLabels :: [LabelEPC]
palletLabels = [IL palletLabel, IL $ GRAI packingHouseCompanyPrefix (AssetType "palletLabel") (SerialNumber "11")]

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

-- Tests that should be implemented in this module:

-- Check Provenance of a labelEPC
-- where I've used head, you need to use map to actually do it for all elements in the list. I've just done one element for illustrative purposes.
-- eventList <- listEvents <labelEPC>
-- let event = head eventList
-- eventInfo <- eventInfo(eventID)
-- (sig, uid) = head (signatures eventInfo)
-- publicKey <- getPublicKey uid
-- assert $ decrypt(sig, publicKey) == (joseText eventInfo)


-- Get all events that relate to a labelEPC
-- eventList <- listEvents <labelEPC>
-- subEvents eventList = [e | e <- eventList, if
-- (eventType e == aggregationEvent || eventType e == transformationEvent)
-- then (map subEvents $ map listEvents (getSubEPCs e)]


