{-# START_FILE src/Lib.hs #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Maybe
import Data.GS1.Event
import Data.GS1.Object
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy
import Data.Either.Combinators
import Data.Time
import Data.ByteString
import GHC.Generics

type UserID = Integer
type EventID = Integer

data User = User {
    userId        :: UserID
  , userFirstName :: String
  , userLastName  :: String
} deriving (Generic, Eq, Show)
instance FromJSON User
instance ToJSON User


-- $(deriveJSON defaultOptions ''User)


data RFIDState = New | InProgress | AwaitingDeploymentToBC | Customer | Finalised
  deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''RFIDState)

data RFIDInfo = RFIDInfo {
  state :: RFIDState,
  owner :: Maybe UserID
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''RFIDInfo)



data NewUser = NewUser {
  phoneNumber :: String,
  firstName :: String,
  lastName :: String,
  company :: String,
  publicKey :: String
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewUser)

data NewObject = NewObject {
  object_userID :: UserID,
  object_epcs :: EPC,
  object_timestamp :: EPCISTime,
  object_timezone:: TimeZone,
  object_objectID :: ObjectID,
  object_location :: GeoLocation
                           } deriving (Show)

data AggregatedObject = AggregatedObject {
  aggObject_userID :: UserID,
  aggObject_objectIDs :: [ObjectID],
  aggObject_timestamp :: EPCISTime,
  aggOject_timezone:: TimeZone,
  aggObject_location :: GeoLocation
                           } deriving (Show)

data TransformationInfo = TransformationInfo {
  transObject_userID :: UserID,
  transObject_objectIDs :: [ObjectID],
  transObject_timestamp :: EPCISTime,
  transObject_timezone:: TimeZone,
  transObject_location :: GeoLocation,
  transObject_inputEPC :: [EPC],
  transObject_inputQuantity :: [Quantity],
  transObject_outputEPC :: [EPC],
  transObject_outputQuantity :: [Quantity]
}

data TransactionInfo = TransactionInfo {
  transaction_userID :: UserID,
  transaction_objectIDs :: [ObjectID],
  transaction_parentID :: Maybe ParentID,
  transaction_bizTransaction :: [BizTransaction],
  transaction_epcs :: [EPC],
  transaction_quantities :: [QuantityElement]
}

data EventInfo = EventInfo {
  event_eventID :: Integer,
  eventType :: EventType,
  rfidState :: RFIDState,
  what :: DWhat,
  why :: DWhy,
  location :: DWhen,
  event_users :: [User]
} deriving (Generic, Eq, Show)

$(deriveJSON defaultOptions ''EventInfo)

{-
instance ToJSON EventInfo
instance FromJSON EventInfo
-}

data SignedEvent = SignedEvent {
  signed_eventID :: Integer,
  signed_eventHash :: ByteString,
  signed_Hashes :: [ByteString],
  signed_users :: [UserID]
}


type API =       "newUser" :> ReqBody '[JSON] NewUser :> Get '[JSON]  UserID
            :<|> "rfid" :>  Capture "RFID" String :> "info" :> Get '[JSON] (Maybe RFIDInfo)
            :<|> "event" :> Capture "eventID" EventID:> "info" :> Get '[JSON] EventInfo

              {-
type API =       "newUser" :>  Get '[JSON] (Maybe UserID)
            :<|> "event" :> Capture "eventID" EventID:> "hash" :> Get '[JSON] SignedEvent
            :<|> "event" :> "sign" :> ReqBody '[JSON] SignedEvent :> Post '[JSON] SignedEvent
            :<|> "contacts" :> Capture "userID" Integer :> Get '[JSON] [User]
            :<|> "contacts" :> "add" :> Capture "userID" Integer :> Get '[JSON] Bool
            :<|> "contacts" :> "remove" :> Capture "userID" Integer :> Get '[JSON] Bool
            :<|> "contacts" :> "search" :> Capture "term" String :> Get '[JSON] [User]
            :<|> "event" :> "list" :> Capture "userID" Integer :> Get '[JSON] EventInfo
            :<|> "event" :> "createObject" :> ReqBody '[JSON] NewObject :> Get '[JSON] ObjectID
            :<|> "event" :> "aggregateObjects" :> ReqBody '[JSON] AggregatedObject :> Get '[JSON] EventInfo
            :<|> "event" :> "start-transaction" :> ReqBody '[JSON] TransactionInfo :> Get '[JSON] EventInfo
            :<|> "event" :> "transformObject" :> ReqBody '[JSON] TransformationInfo :> Get '[JSON] EventInfo
            -- :<|> "login" :>  Put '[JSON] [User]
-}



startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server =  newUser
        :<|> return . rfid
        :<|> return . eventInfo
          {-
        :<|> return eventHash
        :<|> return contactsInfo
        :<|> return contactsAdd
        :<|> return contactsRemove
        :<|> return eventList
        :<|> return eventCreateObject
        :<|> return eventAggregateObjects
        :<|> return eventStartTransaction
        :<|> return eventTransformObject
        -}


newUser ::  NewUser -> Handler UserID
newUser _ = return 1

login :: UserID -> String -> Bool
login = error "implement me"

rfid :: String -> Maybe RFIDInfo
rfid str = Just (RFIDInfo New Nothing)

sampleWhat :: DWhat
sampleWhat = ObjectDWhat Observe [GLN "urn:epc:id:sgtin:0614141" "107346" "2017", GLN "urn:epc:id:sgtin:0614141" "107346" "2018"] []

sampleWhy :: DWhy
sampleWhy = DWhy (Just Arriving) (Just Active)

sampleWhen :: DWhen
sampleWhen = DWhen pt (Just pt) tz
  where
      t = "2017-01-24T13:08:24.11+10:00"
      pt = fromRight' (parseStr2Time t :: Either EPCISTimeError EPCISTime)
      tz = fromRight' (parseStr2TimeZone t :: Either EPCISTimeError TimeZone)


eventInfo :: EventID -> EventInfo
eventInfo eID = EventInfo 1 AggregationEventT New sampleWhat sampleWhy sampleWhen []



eventHash :: EventID -> SignedEvent
eventHash = error "implement me"

contactsInfo :: UserID -> [User]
contactsInfo = error "implement me"

contactsAdd :: UserID -> Bool
contactsAdd = error "implement me"

contactsRemove :: UserID -> Bool
contactsRemove = error "implement me"

eventList :: UserID -> [EventInfo]
eventList = error "implement me"

eventCreateObject :: NewObject -> ObjectID
eventCreateObject = error "implement me"

eventAggregateObjects :: AggregatedObject -> EventInfo
eventAggregateObjects = error "implement me"

eventStartTransaction :: TransactionInfo -> EventInfo
eventStartTransaction = error "implement me"

eventTransformObject :: TransformationInfo -> EventInfo
eventTransformObject = error "implement me"

