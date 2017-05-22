
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}



module Model where


import Servant.Swagger
import Servant.Swagger.UI
import Data.Swagger
import qualified Data.ByteString as ByteString



import Prelude        ()
import Prelude.Compat


import           Control.Monad.IO.Class
import           Control.Monad.Logger (runStderrLoggingT)



import Servant
import Servant.Server.Experimental.Auth()
import Servant.Swagger
import Servant.Swagger.UI

import GHC.TypeLits (KnownSymbol)

import Data.Aeson
import Data.Aeson.TH
import Data.Swagger
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
import Data.String.Conversions

import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict.InsOrd as IOrd
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai

import Database.SQLite.Simple as Sql

import Control.Lens       hiding ((.=))

import GHC.Generics       (Generic)

import System.Environment (getArgs, lookupEnv)

import Text.Read          (readMaybe)
import Data.Text



type UserID = Integer
type EventID = Integer
type EmailAddress = [Word]
type KeyID = Integer
type Password = [Word]

newtype BinaryBlob = BinaryBlob ByteString.ByteString
  deriving (MimeUnrender OctetStream, MimeRender OctetStream)

instance ToSchema BinaryBlob where
  declareNamedSchema _ = pure $ NamedSchema (Just "BinaryBlob") $ binarySchema

instance Sql.FromRow BinaryBlob where
  fromRow = BinaryBlob <$> field


data KeyInfo = KeyInfo {
  userID         :: UserID,
  creationTime   :: Integer,
  revocationTime :: Integer
}deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''KeyInfo)
instance ToSchema KeyInfo

data User = User {
    userId        :: UserID
  , userFirstName :: String
  , userLastName  :: String
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''User)
instance ToSchema User


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
  emailAddress :: String,
  firstName :: String,
  lastName :: String,
  company :: Integer,
  password :: String
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


data SignedEvent = SignedEvent {
  signed_eventID :: Integer,
  signed_eventHash :: BinaryBlob,
  signed_Hashes :: [BinaryBlob],
  signed_users :: [UserID]
}

