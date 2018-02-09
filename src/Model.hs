{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Model where

import qualified Data.ByteString as BS
import           Data.Text as T

import           Servant
import           Servant.Server.Experimental.Auth()
import           Data.Swagger

import           GHC.Generics (Generic)

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Time

import           Data.GS1.EventID
import           Data.GS1.EPC
import           Data.GS1.DWhere
import           Data.GS1.DWhat
import           Data.UUID (UUID)
import           StorageBeam (PrimaryKeyType)

type UserID = PrimaryKeyType

type EmailAddress = T.Text
type KeyID = PrimaryKeyType
type Password = BS.ByteString
type EPCUrn = String

newtype BinaryBlob = BinaryBlob BS.ByteString
  deriving (MimeUnrender OctetStream, MimeRender OctetStream, Generic)


instance ToParamSchema BinaryBlob where
  toParamSchema _ = binaryParamSchema

instance ToSchema BinaryBlob where
  declareNamedSchema _ = pure $ NamedSchema (Just "BinaryBlob") binarySchema

-- instance Sql.FromRow BinaryBlob where
--   fromRow = BinaryBlob <$> field

newtype EventHash = EventHash String
  deriving (Generic, Show, Read, Eq)
$(deriveJSON defaultOptions ''EventHash)
instance ToSchema EventHash

-- instance Sql.FromRow EventHash where
--   fromRow = EventHash <$> field

type JSONTxt = T.Text


-- A signature is an EventHash that's been
-- signed by one of the parties involved in the
-- event.
newtype Signature = Signature String
  deriving (Generic, Show, Read, Eq)
$(deriveJSON defaultOptions ''Signature)
instance ToSchema Signature

-- instance Sql.FromRow Signature where
--   fromRow = Signature <$> field

-- instance Sql.ToRow Signature where
--   toRow (Signature s) = toRow $ Only s

data RSAPublicKey = RSAPublicKey
  {
    rsa_public_n :: Integer,
    rsa_public_e :: Integer
  }
  deriving (Show, Read, Eq, Generic)
-- These are orphaned instances
--
--instance Sql.FromRow RSAPublicKey where
--  fromRow = RSAPublicKey <$> field <$> field

--instance ToParamSchema PublicKey where
--  toParamSchema _ = binaryParamSchema

--instance ToSchema PublicKey where
--  declareNamedSchema _ = pure $ NamedSchema (Just "PublicKey") $ binarySchema

--orphaned instances, I know
$(deriveJSON defaultOptions ''RSAPublicKey)
instance ToSchema RSAPublicKey

data KeyInfo = KeyInfo {
  userID         :: UserID,
  creationTime   :: EPCISTime,
  revocationTime :: Maybe EPCISTime
}deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''KeyInfo)
instance ToSchema KeyInfo

data User = User {
    userId        :: UserID,
    userFirstName :: T.Text,
    userLastName  :: T.Text
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''User)
instance ToSchema User

data EPCState = New | InProgress | AwaitingDeploymentToBC | Customer | Finalised
  deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''EPCState)
instance ToSchema EPCState

-- XXX - do we want to retrieve more information than this?
data EPCInfo = EPCInfo {
  state :: EPCState
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''EPCInfo)
instance ToSchema EPCInfo

type Email = T.Text
data NewUser = NewUser {
  phoneNumber :: T.Text,
  emailAddress :: Email,
  firstName :: T.Text,
  lastName :: T.Text,
  company :: T.Text,
  password :: T.Text
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewUser)
instance ToSchema NewUser


data Business = Business {
  bizID :: UUID,
  bizName :: T.Text,
  gs1CompanyPrefix :: GS1CompanyPrefix,
  function :: T.Text,
  siteName :: T.Text,
  address :: T.Text,
  lat :: Float,
  lng :: Float
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''Business)
instance ToSchema Business

data EventLocation = EventLocation {
  readPoint    :: ReadPointLocation,
  bizLocation  :: BizLocation,
  srcType      :: SrcDestLocation,
  destType     :: SrcDestLocation
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''EventLocation)
instance ToSchema EventLocation


data NewObject = NewObject {
  object_epcs :: LabelEPC,
  object_timestamp :: EPCISTime,
  object_timezone:: TimeZone,
  object_location :: EventLocation,
  object_foreign_event_id :: Maybe EventID
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''NewObject)
instance ToSchema NewObject

data AggregatedObject = AggregatedObject {
  aggObject_objectIDs :: [LabelEPC],
  aggObject_containerID :: LabelEPC,
  aggObject_timestamp :: EPCISTime,
  aggObject_timezone:: TimeZone,
  aggObject_location :: EventLocation,
  aggObject_bizStep :: BizStep,
  aggObject_disposition :: Disposition,
  aggObject_foreign_event_id :: Maybe EventID
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''AggregatedObject)
instance ToSchema AggregatedObject

data DisaggregatedObject = DisaggregatedObject {
  daggObject_objectIDs :: [LabelEPC],
  daggObject_containerID :: LabelEPC,
  daggObject_timestamp :: EPCISTime,
  daggObject_timezone:: TimeZone,
  daggObject_location :: EventLocation,
  daggObject_bizStep :: BizStep,
  daggObject_disposition :: Disposition,
  daggObject_foreign_event_id :: Maybe EventID
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''DisaggregatedObject)
instance ToSchema DisaggregatedObject

data TransformationInfo = TransformationInfo {
  transObject_objectIDs :: [LabelEPC],
  transObject_timestamp :: EPCISTime,
  transObject_timezone:: TimeZone,
  transObject_location :: EventLocation,
  transObject_inputQuantity :: [Quantity],
  transObject_outputObjectID :: [LabelEPC],
  transObject_outputQuantity :: [Quantity],
  transObject_foreign_event_id :: Maybe EventID
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''TransformationInfo)
instance ToSchema TransformationInfo

data TransactionInfo = TransactionInfo {
  transaction_userIDs :: [UserID],
  transaction_objectIDs :: [LabelEPC],
  transaction_parentID :: Maybe ParentID,
  transaction_bizTransaction :: [BizTransaction]
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''TransactionInfo)
instance ToSchema TransactionInfo


data SignedEvent = SignedEvent {
  signed_eventID :: EventID,
  signed_keyID :: Integer,
  signed_signature :: Signature
} deriving (Generic)
$(deriveJSON defaultOptions ''SignedEvent)
instance ToSchema SignedEvent
--instance ToParamSchema SignedEvent where
--  toParamSchema _ = binaryParamSchema

data HashedEvent = HashedEvent {
  hashed_eventID :: EventID,
  hashed_event :: EventHash
} deriving (Generic)
$(deriveJSON defaultOptions ''HashedEvent)
instance ToSchema HashedEvent
