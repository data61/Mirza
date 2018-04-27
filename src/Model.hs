{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | This module is a WIP. Changes will be made to the models very frequently
module Model where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString  as BS
import           Data.Text        as T

import           Data.Swagger

import           Data.UUID        (UUID)
import           GHC.Generics     (Generic)

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC     as EPC
import qualified Data.GS1.Event   as Ev
import           Data.GS1.EventID
import           Servant          (FromHttpApiData, ToHttpApiData)
import           StorageBeam      (PrimaryKeyType)

-- TODO: Should these be in StorageBeam?
newtype UserID = UserID {unUserID :: PrimaryKeyType}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
newtype EmailAddress = EmailAddress {unEmailAddress :: T.Text}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
newtype KeyID = KeyID {unKeyID :: PrimaryKeyType}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
-- Should this be in GS1Combinators?
newtype LabelEPCUrn = LabelEPCUrn {unLabelEPCUrn :: T.Text}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)

-- TODO: Handwrite these instances to comply with their defined syntax
-- For example, emails have their own format, as do LabelEPCUrn

instance ToSchema UserID
instance ToSchema EmailAddress
instance ToSchema KeyID
instance ToSchema LabelEPCUrn

instance ToParamSchema LabelEPCUrn
instance ToParamSchema UserID
instance ToParamSchema EmailAddress
instance ToParamSchema KeyID

deriving instance FromHttpApiData LabelEPCUrn
deriving instance ToHttpApiData LabelEPCUrn
deriving instance FromHttpApiData UserID
deriving instance ToHttpApiData UserID
deriving instance FromHttpApiData EmailAddress
deriving instance ToHttpApiData EmailAddress
deriving instance FromHttpApiData KeyID
deriving instance ToHttpApiData KeyID

-- TODO: This should really be in GS1Combinators
deriving instance ToHttpApiData EventID

newtype Password = Password {unPassword :: BS.ByteString}
  deriving (Show, Eq, Generic)

data Digest = SHA256 | SHA384 | SHA512
  deriving (Show, Generic, Eq, Read)
$(deriveJSON defaultOptions ''Digest)
instance ToSchema Digest

-- XXX - move to the right place
{-
instance HasSqlValueSyntax be String => HasSqlValueSyntax be Digest where
  sqlValueSyntax = autoSqlValueSyntax
instance (IsSql92ColumnSchemaSyntax be) => HasDefaultSqlDataTypeConstraints be Digest

instance FromField Digest where
  fromField f bs = do
    mDigest <- readMaybe <$> fromField f bs
    case mDigest of
      Nothing -> returnError ConversionFailed f "Could not 'read' value for 'Digest"
      Just x -> pure x
-}

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


newtype PEM_RSAPubKey = PEMString String
  deriving (Show, Read, Eq, Generic)
-- These are orphaned instances
--
--instance Sql.FromRow PEM_RSAPubKey where
--  fromRow = PEM_RSAPubKey <$> field <$> field

--instance ToParamSchema PublicKey where
--  toParamSchema _ = binaryParamSchema

--instance ToSchema PublicKey where
--  declareNamedSchema _ = pure $ NamedSchema (Just "PublicKey") $ binarySchema

--orphaned instances, I know
$(deriveJSON defaultOptions ''PEM_RSAPubKey)
instance ToSchema PEM_RSAPubKey

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

data NewUser = NewUser {
  phoneNumber  :: T.Text,
  emailAddress :: EmailAddress,
  firstName    :: T.Text,
  lastName     :: T.Text,
  company      :: GS1CompanyPrefix,
  password     :: T.Text
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewUser)
instance ToSchema NewUser

data SearchFields = SearchFields {
  sUser             :: User,
  sbizName          :: Maybe T.Text,
  sBizId            :: Maybe UUID,
  sGS1CompanyPrefix :: Maybe T.Text,
  sFunction         :: Maybe T.Text,
  sAddress          :: Maybe T.Text
}

data Business = Business {
  bizID    :: EPC.GS1CompanyPrefix,
  bizName  :: T.Text,
  function :: T.Text,
  siteName :: T.Text,
  address  :: T.Text,
  lat      :: Double,
  lng      :: Double
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''Business)
instance ToSchema Business


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

mkObjectEvent :: Ev.Event -> Maybe ObjectEvent
mkObjectEvent
  (Ev.Event Ev.ObjectEventT
    mEid
    (ObjWhat (ObjectDWhat act epcList))
    dwhen dwhy dwhere
  ) = Just $ ObjectEvent mEid act epcList dwhen dwhy dwhere
mkObjectEvent _ = Nothing

fromObjectEvent :: ObjectEvent ->  Ev.Event
fromObjectEvent (ObjectEvent mEid act epcList dwhen dwhy dwhere) =
  Ev.Event
    Ev.ObjectEventT
    mEid
    (ObjWhat (ObjectDWhat act epcList))
    dwhen dwhy dwhere

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

mkAggEvent :: Ev.Event -> Maybe AggregationEvent
mkAggEvent
  (Ev.Event Ev.AggregationEventT
    mEid
    (AggWhat (AggregationDWhat act mParentLabel epcList))
    dwhen dwhy dwhere
  ) = Just $ AggregationEvent mEid act mParentLabel epcList dwhen dwhy dwhere
mkAggEvent _ = Nothing

fromAggEvent :: AggregationEvent ->  Ev.Event
fromAggEvent (AggregationEvent mEid act mParentLabel epcList dwhen dwhy dwhere) =
  Ev.Event
    Ev.AggregationEventT
    mEid
    (AggWhat (AggregationDWhat act mParentLabel epcList))
    dwhen dwhy dwhere

data TransformationEvent = TransformationEvent {
  transf_foreign_event_id  :: Maybe EventID,
  transf_transformation_id :: Maybe TransformationID,
  transf_input_list        :: [InputEPC],
  transf_output_list       :: [OutputEPC],
  transf_when              :: DWhen,
  transf_why               :: DWhy,
  transf_where             :: DWhere
} deriving (Show, Generic)
$(deriveJSON defaultOptions ''TransformationEvent)
instance ToSchema TransformationEvent

mkTransfEvent :: Ev.Event -> Maybe TransformationEvent
mkTransfEvent
  (Ev.Event Ev.TransformationEventT
    mEid
    (TransformWhat (TransformationDWhat mTransfId inputs outputs))
    dwhen dwhy dwhere
  ) = Just $ TransformationEvent mEid mTransfId inputs outputs dwhen dwhy dwhere
mkTransfEvent _ = Nothing

fromTransfEvent :: TransformationEvent ->  Ev.Event
fromTransfEvent (TransformationEvent mEid mTransfId inputs outputs dwhen dwhy dwhere) =
  Ev.Event
    Ev.TransformationEventT
    mEid
    (TransformWhat (TransformationDWhat mTransfId inputs outputs))
    dwhen dwhy dwhere


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

mkTransactEvent :: Ev.Event -> Maybe TransactionEvent
mkTransactEvent
  (Ev.Event Ev.TransactionEventT
    mEid
    (TransactWhat (TransactionDWhat act mParentLabel bizTransactions epcList))
    dwhen dwhy dwhere
  ) = Just $
      TransactionEvent
        mEid act mParentLabel bizTransactions epcList []
        dwhen dwhy dwhere
mkTransactEvent _ = Nothing

fromTransactEvent :: TransactionEvent ->  Ev.Event
fromTransactEvent
  (TransactionEvent
  -- FIXME: userIds is unused?
    mEid act mParentLabel bizTransactions epcList _userIds
    dwhen dwhy dwhere) =
  Ev.Event
    Ev.TransformationEventT
    mEid
    (TransactWhat (TransactionDWhat act mParentLabel bizTransactions epcList))
    dwhen dwhy dwhere

data SignedEvent = SignedEvent {
  signed_eventID   :: EventID,
  signed_keyID     :: KeyID,
  signed_signature :: Signature,
  signed_digest    :: Digest
} deriving (Generic)
$(deriveJSON defaultOptions ''SignedEvent)
instance ToSchema SignedEvent
--instance ToParamSchema SignedEvent where
--  toParamSchema _ = binaryParamSchema

data HashedEvent = HashedEvent {
  hashed_eventID :: EventID,
  hashed_event   :: EventHash
} deriving (Generic)
$(deriveJSON defaultOptions ''HashedEvent)
instance ToSchema HashedEvent
