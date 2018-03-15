{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | This module is a WIP. Changes will be made to the models very frequently
module Model where

import qualified Data.ByteString as BS
import           Data.Text as T
import           Data.Aeson
import           Data.Aeson.TH

import           Servant
import           Servant.Server.Experimental.Auth()
import           Data.Swagger

import           GHC.Generics (Generic)
import           Data.Time
import           Data.UUID (UUID)

import           Data.GS1.EventID
import qualified Data.GS1.Event as Ev
import           Data.GS1.EPC
import           Data.GS1.DWhere
import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhy
import           StorageBeam (PrimaryKeyType)

type UserID = PrimaryKeyType

type EmailAddress = T.Text
type KeyID = PrimaryKeyType
type Password = BS.ByteString
type LabelEPCUrn = String

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

mkObjectEvent :: Ev.Event -> Maybe ObjectEvent
mkObjectEvent
  (Ev.Event Ev.ObjectEventT
    mEid
    (ObjectDWhat act epcList)
    dwhen dwhy dwhere
  ) = Just $ ObjectEvent mEid act epcList dwhen dwhy dwhere
mkObjectEvent _ = Nothing

fromObjectEvent :: ObjectEvent ->  Ev.Event
fromObjectEvent (ObjectEvent mEid act epcList dwhen dwhy dwhere) =
  Ev.Event
    Ev.ObjectEventT
    mEid
    (ObjectDWhat act epcList)
    dwhen dwhy dwhere

data ObjectEvent = ObjectEvent {
  obj_foreign_event_id :: Maybe EventID,
  obj_act              :: Action,
  obj_epc_list         :: [LabelEPC],
  obj_when             :: DWhen,
  obj_why              :: DWhy,
  obj_where            :: DWhere
} deriving (Show, Generic, Eq)
$(deriveJSON defaultOptions ''ObjectEvent)
instance ToSchema ObjectEvent

  -- XXX is it guaranteed to not have a ``recordTime``?
data AggregationEvent = AggregationEvent {
  agg_foreign_event_id :: Maybe EventID,
  agg_act              :: Action,
  agg_parent_label     :: Maybe ParentLabel,
  agg_child_epc_list   :: [LabelEPC],
  agg_when             :: DWhen,
  -- agg_timestamp     :: EPCISTime,
  -- agg_timezone      :: TimeZone,
  agg_why              :: DWhy,
  agg_where            :: DWhere
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''AggregationEvent)
instance ToSchema AggregationEvent

data DisaggregationEvent = DisaggregationEvent {
  disagg_foreign_event_id :: Maybe EventID,
  disagg_act              :: Action,
  disagg_parent_label     :: Maybe ParentLabel,
  disagg_child_epc_list   :: [LabelEPC],
  disagg_when             :: DWhen,
  disagg_why              :: DWhy,
  disagg_where            :: DWhere
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''DisaggregationEvent)
instance ToSchema DisaggregationEvent

data TransformationEvent = TransformationEvent {
  transf_foreign_event_id  :: Maybe EventID,
  transf_transformation_id :: Maybe TransformationID,
  transf_input_list        :: [LabelEPC],
  transf_output_list       :: [LabelEPC],
  transf_when              :: DWhen,
  transf_why               :: DWhy,
  transf_where             :: DWhere
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''TransformationEvent)
instance ToSchema TransformationEvent

data TransactionEvent = TransactionEvent {
  transaction_foreign_event_id     :: Maybe EventID,
  transaction_act                  :: Action,
  transaction_parent_label         :: Maybe ParentLabel,
  transaction_biz_transaction_list :: [BizTransaction],
  transaction_epc_list             :: [LabelEPC],
  transaction_user_ids             :: [UserID],
  transaction_when                 :: DWhen,
  transaction_why                  :: DWhy,
  transaction_where                :: DWhere
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''TransactionEvent)
instance ToSchema TransactionEvent

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
