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

import           Mirza.Common.Types as Common

import           GHC.Generics       (Generic)

import           Data.Aeson
import           Data.Aeson.TH

import           Data.Swagger
import           Servant            (FromHttpApiData, ToHttpApiData)


import           Data.Text          (Text)

import           Data.Time          (UTCTime)

newtype BlockchainTransactionHash = BlockchainTransactionHash Text
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''BlockchainTransactionHash)
instance ToSchema BlockchainTransactionHash
deriving instance FromHttpApiData BlockchainTransactionHash
deriving instance ToHttpApiData BlockchainTransactionHash

newtype IdentifierHash = IdentifierHash Text
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''IdentifierHash)
instance ToSchema IdentifierHash
deriving instance FromHttpApiData IdentifierHash
deriving instance ToHttpApiData IdentifierHash

newtype EventHash = EventHash Text
  deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''EventHash)
instance ToSchema EventHash
deriving instance FromHttpApiData EventHash
deriving instance ToHttpApiData EventHash

data EventInsertionResponse = EventInsertionResponse
  { respEventHash :: EventHash
  , respTimeStamp :: UTCTime
  } deriving (Eq, Show, Generic)
$(deriveJSON defaultOptions ''EventInsertionResponse)
instance ToSchema EventInsertionResponse
