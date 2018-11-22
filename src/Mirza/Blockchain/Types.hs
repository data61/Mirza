{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Mirza.Blockchain.Types
  ( module Mirza.Blockchain.Types
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

import           Crypto.JOSE                as JOSE hiding (Digest)
import           Crypto.JOSE.Types          (Base64Octets)
import           Crypto.Scrypt              (ScryptParams)

import           Servant                    (FromHttpApiData, ToHttpApiData)
import           Servant.Client             (ClientEnv (..), ServantError (..))

import           Control.Lens

import           GHC.Generics               (Generic)

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString            as BS
import           Data.List.NonEmpty         (NonEmpty)
import           Data.Pool                  as Pool
import           Data.Swagger
import           Data.Text                  (Text)

import           Katip                      as K

import           Data.Time                  (UTCTime)


-- *****************************************************************************
-- Context Types
-- *****************************************************************************

data BCContext = BCContext
  { _bcEnvType          :: EnvType
  , _bcDbConnPool       :: Pool Connection
  , _bcScryptPs         :: ScryptParams
  , _bcKatipLogEnv      :: K.LogEnv
  , _bcKatipLogContexts :: K.LogContexts
  , _bcKatipNamespace   :: K.Namespace
  , _bcBRClientEnv      :: ClientEnv
  }
$(makeLenses ''BCContext)

instance HasEnvType BCContext where envType = bcEnvType
instance HasConnPool BCContext where connPool = bcDbConnPool
instance HasScryptParams BCContext where scryptParams = bcScryptPs
instance HasBRClientEnv BCContext where clientEnv = bcBRClientEnv
instance HasKatipLogEnv BCContext where katipLogEnv = bcKatipLogEnv
instance HasKatipContext BCContext where
  katipContexts = bcKatipLogContexts
  katipNamespace = bcKatipNamespace

successHealthResponseText :: Text
successHealthResponseText = "Status OK"

data HealthResponse = HealthResponse
  deriving (Show, Eq, Read, Generic)
instance ToSchema HealthResponse
instance ToJSON HealthResponse where
  toJSON _ = toJSON successHealthResponseText
instance FromJSON HealthResponse where
  parseJSON (String value)
    | value == successHealthResponseText = pure HealthResponse
    | otherwise                          = fail "Invalid health response string."
  parseJSON value                        = typeMismatch "HealthResponse" value

newtype BlockchainTransactionHash = BlockchainTransactionHash Text
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''BlockchainTransactionHash)
instance ToSchema BlockchainTransactionHash


newtype IdentifierHash = IdentifierHash Text
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''IdentifierHash)
instance ToSchema IdentifierHash

newtype EventHash = EventHash Text
  deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''EventHash)
instance ToSchema EventHash

data EventInsertionResponse = EventInsertionResponse
  { respEventHash :: EventHash
  , respTimeStamp :: UTCTime
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''EventInsertionResponse)
instance ToSchema EventInsertionResponse
