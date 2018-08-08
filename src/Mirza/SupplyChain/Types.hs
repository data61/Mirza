{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Mirza.SupplyChain.Types
  ( module Mirza.SupplyChain.Types
  , module Common
  )
  where

import           Mirza.Common.Types         as Common

import           Data.GS1.DWhat
import           Data.GS1.DWhen
import           Data.GS1.DWhere
import           Data.GS1.DWhy
import           Data.GS1.EPC               as EPC
import qualified Data.GS1.Event             as Ev
import           Data.GS1.EventId           as EvId

import           Database.PostgreSQL.Simple (Connection, SqlError)

import           Crypto.Scrypt              (ScryptParams)

import           Servant                    (FromHttpApiData, ToHttpApiData)

import           Control.Lens

import           GHC.Generics               (Generic)

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString            as BS
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Pool                  as Pool
import           Data.Swagger
import           Data.Text                  (Text)

import           Katip                      as K


-- *****************************************************************************
-- Context Types
-- *****************************************************************************

data SCSContext = SCSContext
  { _scsEnvType          :: EnvType
  , _scsDbConnPool       :: Pool Connection
  , _scsScryptPs         :: ScryptParams
  , _scsKatipLogEnv      :: K.LogEnv
  , _scsKatipLogContexts :: K.LogContexts
  , _scsKatipNamespace   :: K.Namespace
  -- , port    :: Word16
  }
$(makeLenses ''SCSContext)

instance HasEnvType SCSContext where envType = scsEnvType
instance HasConnPool SCSContext where connPool = scsDbConnPool
instance HasScryptParams SCSContext where scryptParams = scsScryptPs
instance HasKatipLogEnv SCSContext where katipLogEnv = scsKatipLogEnv
instance HasKatipContext SCSContext where
  katipContexts = scsKatipLogContexts
  katipNamespace = scsKatipNamespace


-- *****************************************************************************
-- User Types
-- *****************************************************************************

data User = User {
  userId        :: UserId,
  userFirstName :: Text,
  userLastName  :: Text
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''User)
instance ToSchema User

data NewUser = NewUser {
  newUserPhoneNumber  :: Text,
  newUserEmailAddress :: EmailAddress,
  newUserFirstName    :: Text,
  newUserLastName     :: Text,
  newUserCompany      :: GS1CompanyPrefix,
  newUserPassword     :: Text
} deriving (Generic, Eq, Show)
$(deriveJSON defaultOptions ''NewUser)
instance ToSchema NewUser



-- *****************************************************************************
-- GS1 Types
-- *****************************************************************************

-- TODO: This should really be in GS1Combinators
deriving instance ToHttpApiData EventId

-- Should this be in GS1Combinators?
newtype LabelEPCUrn = LabelEPCUrn {getLabelEPCUrn :: Text}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
instance ToSchema LabelEPCUrn
instance ToParamSchema LabelEPCUrn
deriving instance FromHttpApiData LabelEPCUrn
deriving instance ToHttpApiData LabelEPCUrn

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



-- *****************************************************************************
-- Event Types
-- *****************************************************************************
-- TODO: The factory functions should probably be removed from here.

newtype EventOwner  = EventOwner UserId deriving(Generic, Show, Eq, Read)

data ObjectEvent = ObjectEvent {
  obj_foreign_event_id :: Maybe EventId,
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
  agg_foreign_event_id :: Maybe EventId,
  agg_act              :: Action,
  agg_parent_label     :: Maybe ParentLabel,
  agg_child_epc_list   :: [LabelEPC],
  agg_when             :: DWhen,
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
  transf_foreign_event_id  :: Maybe EventId,
  transf_transformation_id :: Maybe TransformationId,
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
  transaction_foreign_event_id     :: Maybe EventId,
  transaction_act                  :: Action,
  transaction_parent_label         :: Maybe ParentLabel,
  transaction_biz_transaction_list :: [BizTransaction],
  transaction_epc_list             :: [LabelEPC],
  transaction_user_ids             :: [UserId],
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


newtype SigningUser = SigningUser UserId deriving(Generic, Show, Eq, Read)

newtype EventHash = EventHash String
  deriving (Generic, Show, Read, Eq)
$(deriveJSON defaultOptions ''EventHash)
instance ToSchema EventHash

-- A signature is an EventHash that's been
-- signed by one of the parties involved in the
-- event.

newtype Signature = Signature String
  deriving (Generic, Show, Read, Eq)
$(deriveJSON defaultOptions ''Signature)
instance ToSchema Signature

data BlockchainPackage = BlockchainPackage EventHash (NonEmpty (Signature, UserId))
  deriving (Show, Read, Eq, Generic)
$(deriveJSON defaultOptions ''BlockchainPackage)
instance ToSchema BlockchainPackage

-- instance Sql.FromRow Signature where
--   fromRow = Signature <$> field

-- instance Sql.ToRow Signature where
--   toRow (Signature s) = toRow $ Only s

newtype PEM_RSAPubKey = PEMString String
  deriving (Show, Read, Eq, Generic)
$(deriveJSON defaultOptions ''PEM_RSAPubKey)
instance ToSchema PEM_RSAPubKey
-- These are orphaned instances
--
--instance Sql.FromRow PEM_RSAPubKey where
--  fromRow = PEM_RSAPubKey <$> field <$> field

--instance ToParamSchema PublicKey where
--  toParamSchema _ = binaryParamSchema

--instance ToSchema PublicKey where
--  declareNamedSchema _ = pure $ NamedSchema (Just "PublicKey") $ binarySchema

--orphaned instances, I know

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


data SignedEvent = SignedEvent {
  signed_eventId   :: EventId,
  signed_keyId     :: KeyId,
  signed_signature :: Signature,
  signed_digest    :: Digest
} deriving (Generic)
$(deriveJSON defaultOptions ''SignedEvent)
instance ToSchema SignedEvent
--instance ToParamSchema SignedEvent where
--  toParamSchema _ = binaryParamSchema

data HashedEvent = HashedEvent {
  hashed_eventId :: EventId,
  hashed_event   :: EventHash
} deriving (Generic)
$(deriveJSON defaultOptions ''HashedEvent)
instance ToSchema HashedEvent



-- *****************************************************************************
-- Error Types
-- *****************************************************************************

-- | Top level application error type, which combines errors from several
-- domains. Currently only `ServiceError` is contained by AppError, but as this
-- is broken into smaller error domains and other domains are added more
-- constructors will be added.
newtype AppError = AppError ServiceError deriving (Show)

data ServerError = ServerError (Maybe BS.ByteString) Text
                   deriving (Show, Eq, Generic, Read)

-- | A sum type of errors that may occur in the Service layer
data ServiceError
  = InvalidSignature      String
  | BlockchainSendFailed  ServerError
  | InvalidEventId        EventId
  | InvalidKeyId          KeyId
  | InvalidUserId         UserId
  | InvalidRSAKeyInDB     Text -- when the key already existing in the DB is wrong
  | InvalidDigest         Digest
  | InsertionFail         ServerError Text
  | EventPermissionDenied UserId EvId.EventId
  | EmailExists           ServerError EmailAddress
  | EmailNotFound         EmailAddress
  | AuthFailed            EmailAddress
  | UserNotFound          EmailAddress
  | ParseError            EPC.ParseFailure
  | BackendErr            Text -- fallback
  | DatabaseError         SqlError
  deriving (Show, Eq, Generic)
$(makeClassyPrisms ''ServiceError)

instance AsServiceError AppError where
  _ServiceError = prism' AppError (\(AppError se) -> Just se)

instance AsSqlError ServiceError where
  _SqlError = _DatabaseError

instance AsSqlError AppError where
  _SqlError = _DatabaseError
