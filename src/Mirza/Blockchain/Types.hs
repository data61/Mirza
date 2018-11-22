{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

module Mirza.Blockchain.Types
  ( module Mirza.Blockchain.Types
  , module Common
  )
  where

import           Mirza.Common.Types as Common

import           GHC.Generics       (Generic)

import           Data.Aeson         (FromJSON (..), ToJSON (..))
import           Data.Aeson.TH

import           Data.Swagger
import           Servant            (FromHttpApiData (..), ToHttpApiData (..))


import           Data.Text          (Text)

import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time          (UTCTime)

import           Data.Proxy         (Proxy (..))

import           Control.Lens

import           Data.ByteString    (ByteString)

newtype BlockchainTransactionHash = BlockchainTransactionHash {getBlockchainTransactionHash :: ByteString}
  deriving (Eq, Show, Generic)

instance ToJSON BlockchainTransactionHash where
  toJSON (BlockchainTransactionHash h) = toJSON . decodeUtf8 $ h
instance FromJSON BlockchainTransactionHash where
  parseJSON v = BlockchainTransactionHash . encodeUtf8 <$> parseJSON v
instance ToSchema BlockchainTransactionHash where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
    <&> name ?~ "BlockchainTransactionHash"
    <&> schema . description ?~ "Blockchain Transaction Hash"
instance FromHttpApiData BlockchainTransactionHash where
  parseUrlPiece t = fmap (BlockchainTransactionHash . encodeUtf8) (parseUrlPiece t)
instance ToHttpApiData BlockchainTransactionHash where
  toUrlPiece (BlockchainTransactionHash h) = toUrlPiece . decodeUtf8 $ h


newtype IdentifierHash = IdentifierHash {getIdentifierHash :: ByteString}
  deriving (Eq, Show, Generic)

instance ToJSON IdentifierHash where
  toJSON (IdentifierHash h) = toJSON . decodeUtf8 $ h
instance FromJSON IdentifierHash where
  parseJSON v = IdentifierHash . encodeUtf8 <$> parseJSON v
instance ToSchema IdentifierHash where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
    <&> name ?~ "IdentifierHash"
    <&> schema . description ?~ "Identifier Hash"
instance FromHttpApiData IdentifierHash where
  parseUrlPiece t = fmap (IdentifierHash . encodeUtf8) (parseUrlPiece t)
instance ToHttpApiData IdentifierHash where
  toUrlPiece (IdentifierHash h) = toUrlPiece . decodeUtf8 $ h


newtype EventHash = EventHash {getEventHash :: ByteString}
  deriving (Eq, Show, Generic)
instance ToJSON EventHash where
  toJSON (EventHash h) = toJSON . decodeUtf8 $ h
instance FromJSON EventHash where
  parseJSON v = EventHash . encodeUtf8 <$> parseJSON v
instance ToSchema EventHash where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
    <&> name ?~ "EventHash"
    <&> schema . description ?~ "Identifier Hash"
instance FromHttpApiData EventHash where
  parseUrlPiece t = fmap (EventHash . encodeUtf8) (parseUrlPiece t)
instance ToHttpApiData EventHash where
  toUrlPiece (EventHash h) = toUrlPiece . decodeUtf8 $ h


data EventInsertionResponse = EventInsertionResponse
  { respEventHash :: EventHash
  , respTimeStamp :: UTCTime
  } deriving (Eq, Show, Generic)
instance ToSchema EventInsertionResponse
$(deriveJSON defaultOptions ''EventInsertionResponse)
