{-# START_FILE src/Lib.hs #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Swagger
import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.Server                   (BasicAuthCheck (BasicAuthCheck),
                                         BasicAuthResult( Authorized
                                                        , Unauthorized
                                                        ),
                                         Context ((:.), EmptyContext),
                                         err401, err403, errBody, Server,
                                         serveWithContext, Handler)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                         mkAuthHandler)
import Servant.Server.Experimental.Auth()
import Servant.Swagger
import GHC.TypeLits (KnownSymbol)

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
import Data.Text                        (Text)
import Data.ByteString
import GHC.Generics


import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Encode.Pretty   (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Proxy
import           Data.Swagger
import           Data.Text                  (Text)
import           Data.Time                  (UTCTime (..), fromGregorian)
import           Data.Typeable              (Typeable)
import           GHC.Generics
import           Servant
import           Servant.Swagger
import qualified Data.HashMap.Strict.InsOrd as IOrd


type UserID = Integer

type EventID = Integer

newtype BinaryBlob = BinaryBlob ByteString
  deriving (MimeUnrender OctetStream, MimeRender OctetStream)

{-
instance ToSchema BinaryBlob where
  declareNamedSchema _ = pure (Just "BinaryBlob", binarySchema)
  -}

instance ToSchema BinaryBlob where
  declareNamedSchema _ = pure $ NamedSchema (Just "BinaryBlob") $ binarySchema

{-
instance ToSchema BinaryBlob where
  declareNamedSchema _ = pure (Just "BinaryBlob", s)
    where
      s = mempty
        & schemaType .~ SwaggerString
        & schemaFormat ?~ "binary"
        -}

data User = User {
    userId        :: UserID
  , userFirstName :: String
  , userLastName  :: String
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''User)
--instance ToParamSchema User
instance ToSchema User

instance ToSchema Swagger

data RFIDState = New | InProgress | AwaitingDeploymentToBC | Customer | Finalised
  deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''RFIDState)
instance ToSchema RFIDState

data RFIDInfo = RFIDInfo {
  state :: RFIDState,
  owner :: Maybe UserID
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''RFIDInfo)
instance ToSchema RFIDInfo


data NewUser = NewUser {
  phoneNumber :: String,
  firstName :: String,
  lastName :: String,
  company :: String,
  publicKey :: String
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewUser)
instance ToSchema NewUser

data NewObject = NewObject {
  object_userID :: UserID,
  object_epcs :: EPC,
  object_timestamp :: EPCISTime,
  object_timezone:: TimeZone,
  object_objectID :: ObjectID,
  object_location :: GeoLocation
                           } deriving (Show, Generic)
$(deriveJSON defaultOptions ''NewObject)
instance ToSchema NewObject

data AggregatedObject = AggregatedObject {
  aggObject_userID :: UserID,
  aggObject_objectIDs :: [ObjectID],
  aggObject_timestamp :: EPCISTime,
  aggOject_timezone:: TimeZone,
  aggObject_location :: GeoLocation
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''AggregatedObject)
instance ToSchema AggregatedObject

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
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''TransformationInfo)
instance ToSchema TransformationInfo



data TransactionInfo = TransactionInfo {
  transaction_userID :: UserID,
  transaction_objectIDs :: [ObjectID],
  transaction_parentID :: Maybe ParentID,
  transaction_bizTransaction :: [BizTransaction],
  transaction_epcs :: [EPC],
  transaction_quantities :: [QuantityElement]
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''TransactionInfo)
instance ToSchema TransactionInfo

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
instance ToSchema EventInfo


{-
instance ToJSON EventInfo
instance FromJSON EventInfo
-}

data SignedEvent = SignedEvent {
  signed_eventID :: Integer,
  signed_eventHash :: BinaryBlob,
  signed_Hashes :: [BinaryBlob],
  signed_users :: [UserID]
}
-- instance ToSchema SignedEvent
-- $(deriveJSON defaultOptions ''SignedEvent)
-- $(deriveJSON defaultOptions ''BinaryBlob)


type PrivateAPI =       "newUser" :> ReqBody '[JSON] NewUser :> Get '[JSON]  UserID
            :<|> "rfid" :>  Capture "RFID" String :> "info" :> Get '[JSON] (Maybe RFIDInfo)
            :<|> "event" :> Capture "eventID" EventID:> "info" :> Get '[JSON] EventInfo
            :<|> "contacts" :> Capture "userID" Integer :> Get '[JSON] [User]
            :<|> "contacts" :> "add" :> Capture "userID" Integer :> Get '[JSON] Bool
            :<|> "contacts" :> "remove" :> Capture "userID" Integer :> Get '[JSON] Bool
            :<|> "contacts" :> "search" :> Capture "term" String :> Get '[JSON] [User]
            :<|> "event" :> "list" :> Capture "userID" Integer :> Get '[JSON] [EventInfo]
            :<|> "event" :> "createObject" :> ReqBody '[JSON] NewObject :> Get '[JSON] ObjectID
            :<|> "event" :> "aggregateObjects" :> ReqBody '[JSON] AggregatedObject :> Get '[JSON] EventInfo
            :<|> "event" :> "start-transaction" :> ReqBody '[JSON] TransactionInfo :> Get '[JSON] EventInfo
            :<|> "event" :> "transformObject" :> ReqBody '[JSON] TransformationInfo :> Get '[JSON] EventInfo
            :<|> "key" :> "add" :>  ReqBody '[OctetStream] BinaryBlob :> Get '[JSON] (Bool, String)
            :<|> "key" :> "get" :> Capture "userID" UserID :> Get '[OctetStream] BinaryBlob

              {-
type API = :<|> "event" :> "sign" :> ReqBody '[JSON] SignedEvent :> Post '[JSON] SignedEvent
            :<|> "event" :> Capture "eventID" EventID:> "hash" :> Get '[JSON] SignedEvent
            -- :<|> "login" :>  Put '[JSON] [User]
-}

type PublicAPI = "login" :> Get '[JSON] User
-- type PublicAPI =       "newUser" :> ReqBody '[JSON] NewUser :> Get '[JSON]  UserID
--

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger




-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: BasicAuthCheck User
authCheck =
  let check (BasicAuthData username password) =
        if username == "servant" && password == "server"
        then return (Authorized (sampleUser))
        else return Unauthorized
  in BasicAuthCheck check


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serveWithContext api basicAuthServerContext server

api :: Proxy API
api = Proxy


type API =  BasicAuth "foo-realm" User :> PrivateAPI :<|> PublicAPI :<|> SwaggerAPI
-- type API = ServerAPI :<|> SwaggerAPI
{-
instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
-}

instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ =
    let
      authSchemes = IOrd.singleton "basic" $ SecurityScheme SecuritySchemeBasic Nothing
      securityRequirements = [SecurityRequirement $ IOrd.singleton "basic" []]
    in
      toSwagger (Proxy :: Proxy sub)
      & securityDefinitions .~ authSchemes
      & allOperations . security .~ securityRequirements



publicServer :: Server PublicAPI
publicServer =  login

privateServer :: User -> Server PrivateAPI
privateServer _ =  newUser
        :<|> return . rfid
        :<|> return . eventInfo
        :<|> return . contactsInfo
        :<|> return . contactsAdd
        :<|> return . contactsRemove
        :<|> return . contactsSearch
        :<|> return . eventList
        :<|> return . eventCreateObject
        :<|> return . eventAggregateObjects
        :<|> return . eventStartTransaction
        :<|> return . eventTransformObject
        :<|> return . addPublicKey
        :<|> return . getPublicKey

          {-
        :<|> return . eventHash
        -}

--instance ToSchema API

-- instance ToParamSchema TodoId
-- instance ToSchema TodoId

-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger api
  & info.title   .~ "Supplychain Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")


server :: Server API
server = privateServer :<|> publicServer :<|> return serveSwaggerAPI



-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: Servant.Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext


addPublicKey :: BinaryBlob -> (Bool, String)
addPublicKey   sig = (True, "Success")

getPublicKey :: UserID -> BinaryBlob
getPublicKey userID = BinaryBlob empty

newUser ::  NewUser -> Handler UserID
newUser _ = return 1

login :: Handler User
login = return sampleUser

rfid :: String -> Maybe RFIDInfo
rfid str = Just (RFIDInfo New Nothing)

sampleUser :: User
sampleUser =  User 1 "Sara" "Falamaki"


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
eventHash eID = SignedEvent eID (BinaryBlob empty) [(BinaryBlob empty)] [1,2]



contactsInfo :: UserID -> [User]
contactsInfo uID = []

contactsAdd :: UserID -> Bool
contactsAdd uID = False

contactsRemove :: UserID -> Bool
contactsRemove uID = False

contactsSearch :: String -> [User]
contactsSearch term = []

eventList :: UserID -> [EventInfo]
eventList uID = [(eventInfo 1)]

eventCreateObject :: NewObject -> ObjectID
eventCreateObject newObject = "newObjectID"

eventAggregateObjects :: AggregatedObject -> EventInfo
eventAggregateObjects _ = eventInfo 1

eventStartTransaction :: TransactionInfo -> EventInfo
eventStartTransaction _ = eventInfo 1

eventTransformObject :: TransformationInfo -> EventInfo
eventTransformObject _ = eventInfo 1

