{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Mirza.SupplyChain.Types
  ( module Mirza.SupplyChain.Types
  , module Common
  )
  where

import           Mirza.Common.GS1BeamOrphans (LabelType)
import           Mirza.Common.Types          as Common

import           Data.GS1.DWhat
import           Data.GS1.EPC                as EPC
import qualified Data.GS1.Event              as Ev
import           Data.GS1.EventId            as EvId

import           Database.PostgreSQL.Simple  (Connection, SqlError)

import           Crypto.JOSE                 as JOSE hiding (Digest)
import           Crypto.JOSE.Types           (Base64Octets)

import           Servant                     (ToHttpApiData)
import           Servant.Client              (ClientEnv (..), ServantError (..))

import           Control.Lens                hiding ((.=))
import           Data.Function               (on)

import           GHC.Generics                (Generic)

import           Data.Aeson

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL

import           Data.Foldable               (toList)
import qualified Data.HashMap.Strict         as HashMap
import           Data.List.NonEmpty          (NonEmpty, sortBy)

import           Data.Scientific             (FPFormat (..), formatScientific)

import           Data.Pool                   as Pool
import           Data.Swagger
import           Data.Text                   (Text, unpack)

import           Katip                       as K

import           Mirza.OrgRegistry.Types     (AsORError (..), ORError)

import qualified Text.JSON.Canonical         as CJSON

-- *****************************************************************************
-- Context Types
-- *****************************************************************************

data SCSContext = SCSContext
  { _scsEnvType          :: EnvType
  , _scsDbConnPool       :: Pool Connection
  , _scsKatipLogEnv      :: K.LogEnv
  , _scsKatipLogContexts :: K.LogContexts
  , _scsKatipNamespace   :: K.Namespace
  , _scsORClientEnv      :: ClientEnv
  }
$(makeLenses ''SCSContext)

instance HasEnvType SCSContext where envType = scsEnvType
instance HasConnPool SCSContext where connPool = scsDbConnPool
instance HasORClientEnv SCSContext where clientEnv = scsORClientEnv
instance HasKatipLogEnv SCSContext where katipLogEnv = scsKatipLogEnv
instance HasKatipContext SCSContext where
  katipContexts = scsKatipLogContexts
  katipNamespace = scsKatipNamespace

data LabelWithType = LabelWithType
  { getLabelType :: Maybe LabelType
  , getLabel     :: LabelEPC
  } deriving (Show, Eq)
$(makeLenses ''LabelWithType)

deriving instance ToHttpApiData EventId

newtype EventHash = EventHash String
  deriving (Generic, Show, Read, Eq)
instance ToSchema EventHash

-- A signature is an EventHash that's been
-- signed by one of the parties involved in the event.
type Signature' = Signature () JWSHeader

newtype EventToSign = EventToSign { getEventToSign :: BS.ByteString }
  deriving (Show, Eq, Generic)

data BlockchainPackage = BlockchainPackage Ev.Event (NonEmpty (UserId, SignedEvent))
  deriving (Show, Eq, Generic)

mkBlockchainPackage :: Ev.Event -> NonEmpty (UserId, SignedEvent) -> BlockchainPackage
mkBlockchainPackage e = BlockchainPackage e . sortBy (compare `on` fst)

instance ToJSON BlockchainPackage where
  toJSON (BlockchainPackage e xs) = object [ "event" .= e, "signatures" .= xs ]

newtype Canonical a = Canonical a
  deriving (Show, Eq)

toCanonicalJSON :: ToJSON a => a -> BS.ByteString
toCanonicalJSON = BSL.toStrict
                  . CJSON.renderCanonicalJSON
                  . runIdentity
                  . CJSON.toJSON
                  . Canonical
                  . toJSON

instance (Monad m, ToJSON a) => CJSON.ToJSON m (Canonical a) where
  toJSON (Canonical a) = pure $ toCJSON $ toJSON a
    where
      toCJSON :: Value -> CJSON.JSValue
      toCJSON (Object x) = CJSON.JSObject $ fmap (bimap unpack toCJSON) $ HashMap.toList x
      toCJSON (Array x) = CJSON.JSArray $ fmap toCJSON $ toList x
      toCJSON (String x) = CJSON.JSString $ unpack x
      toCJSON (Number x) = CJSON.JSString $ formatScientific Fixed Nothing x
      toCJSON (Bool x) = CJSON.JSBool x
      toCJSON Null = CJSON.JSNull

data SignedEvent = SignedEvent {
  signed_eventId   :: EventId,
  signed_keyId     :: ORKeyId,
  signed_signature :: CompactJWS JWSHeader
  } deriving (Generic, Show, Eq)

instance FromJSON SignedEvent where
  parseJSON = withObject "SignedEvent" $ \o -> SignedEvent
    <$> o .: "event_id"
    <*> o .: "key_id"
    <*> o .: "signature"

instance ToJSON SignedEvent where
  toJSON (SignedEvent evId keyId sig) = object
    [ "event_id" .= evId
    , "key_id" .= keyId
    , "signature" .= sig
    ]

instance ToSchema SignedEvent

data HashedEvent = HashedEvent {
  hashed_eventId :: EventId,
  hashed_event   :: EventHash
  } deriving (Generic)
instance ToSchema HashedEvent

newtype BlockchainId = BlockchainId Text
  deriving (Show, Generic, Eq)
instance ToSchema BlockchainId

data EventBlockchainStatus
  = Sent -- BlockchainId -- commented out for the moment because ToSchema cannot be auto-derived
  | ReadyAndWaiting
  | SendFailed -- sending was attempted but failed
  | NeedMoreSignatures
  deriving (Show, Generic, Eq)

instance FromJSON EventBlockchainStatus where
  parseJSON = withText "EventBlockchainStatus" $ \case
    "sent" -> pure Sent
    "ready_and_waiting" -> pure ReadyAndWaiting
    "send_failed" -> pure SendFailed
    "need_more_signatures" -> pure NeedMoreSignatures
    _ -> fail "Invalid Blockchain Status"

instance ToJSON EventBlockchainStatus where
  toJSON Sent               = String "sent"
  toJSON ReadyAndWaiting    = String "ready_and_waiting"
  toJSON SendFailed         = String "send_failed"
  toJSON NeedMoreSignatures = String "need_more_signatures"

instance ToSchema EventBlockchainStatus


data EventInfo = EventInfo {
  eventInfoEvent            :: Ev.Event,
  eventToSign               :: Base64Octets, --this is what users would be required to sign
  eventInfoBlockChainStatus :: EventBlockchainStatus
} deriving (Show, Eq, Generic)

instance FromJSON EventInfo where
  parseJSON = withObject "EventInfo" $ \o ->  EventInfo
    <$> o .: "event"
    <*> o .: "event_to_sign"
    <*> o .: "blockchain_status"

instance ToJSON EventInfo where
  toJSON (EventInfo ev evToSign bcStatus) = object
    [ "event" .= ev
    , "event_to_sign" .= evToSign
    , "blockchain_status" .= bcStatus
    ]

instance ToSchema EventInfo


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
  = InvalidSignature         String
  | SigVerificationFailure   String
  | BlockchainSendFailed     ServerError
  | InvalidEventId           EventId
  | InvalidKeyId             ORKeyId
  | JOSEError                JOSE.Error
  | ParseError               EPC.ParseFailure
  | BackendErr               Text -- fallback
  | DatabaseError            SqlError
  | EventExists              Ev.Event
  | UnmatchedUniqueViolation SqlError
  | ServantErr               ServantError
  | ORServerError            ORError -- Error occured when a call was made to OR
  deriving (Show, Generic)
$(makeClassyPrisms ''ServiceError)

instance AsORError AppError where
  _ORError = prism' (AppError . ORServerError)
              (\err -> case err of
                (AppError (ORServerError e)) -> Just e
                _                            -> Nothing
              )

instance AsServiceError AppError where
  _ServiceError = prism' AppError (\(AppError se) -> Just se)

instance AsSqlError ServiceError where
  _SqlError = _DatabaseError

instance AsSqlError AppError where
  _SqlError = _DatabaseError

instance AsServantError ServantError where
  _ServantError = id

instance AsServantError ServiceError where
  _ServantError = _ServantErr

instance AsServantError AppError where _ServantError = _ServantErr

instance JOSE.AsError ServiceError where _Error = _JOSEError
instance JOSE.AsError AppError where _Error = _JOSEError
