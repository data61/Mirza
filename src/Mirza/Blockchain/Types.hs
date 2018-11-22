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



import           Data.Text          (Text)

import           Data.Time          (UTCTime)

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
