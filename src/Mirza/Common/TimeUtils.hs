{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Mirza.Common.TimeUtils (
    CreationTime(..), RevocationTime(..), ExpirationTime(..)
  , DBTimestamp, ModelTimestamp, fromDbTimestamp, toDbTimestamp
  ) where

import           GHC.Generics        (Generic)
import           Servant             (FromHttpApiData (..), ToHttpApiData (..))

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Swagger

import           Data.Text           (Text)

import           Data.Time           (LocalTime, UTCTime, ZonedTime (..),
                                      utcToZonedTime)
import           Data.Time.Clock     (getCurrentTime)
import           Data.Time.LocalTime (LocalTime, localTimeToUTC, utc,
                                      utcToLocalTime)


class DBTimestamp t where
  toDbTimestamp :: t -> LocalTime
class ModelTimestamp t where
  fromDbTimestamp :: LocalTime -> t

-- | Reads back the ``LocalTime`` in UTCTime (with an offset of 0)
-- And wraps it in a custom constructor (newtype wrappers around UTCTime)
onLocalTime :: (UTCTime -> t) -> LocalTime -> t
onLocalTime c t = c (localTimeToUTC utc t)

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
_toZonedTime :: UTCTime -> ZonedTime
_toZonedTime = utcToZonedTime utc

toLocalTime :: UTCTime -> LocalTime
toLocalTime = utcToLocalTime utc


newtype CreationTime = CreationTime {unCreationTime :: UTCTime}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON, Ord)
instance ToSchema CreationTime
instance ToParamSchema CreationTime
deriving instance FromHttpApiData CreationTime
deriving instance ToHttpApiData CreationTime
instance DBTimestamp CreationTime where
  toDbTimestamp (CreationTime t) = toLocalTime t
instance ModelTimestamp CreationTime where
  fromDbTimestamp = onLocalTime CreationTime


newtype RevocationTime = RevocationTime {unRevocationTime :: UTCTime}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON, Ord)
instance ToSchema RevocationTime
instance ToParamSchema RevocationTime
deriving instance FromHttpApiData RevocationTime
deriving instance ToHttpApiData RevocationTime
instance DBTimestamp RevocationTime where
  toDbTimestamp (RevocationTime t) = toLocalTime t
instance ModelTimestamp RevocationTime where
  fromDbTimestamp = onLocalTime RevocationTime


newtype ExpirationTime = ExpirationTime {unExpirationTime :: UTCTime}
  deriving (Show, Eq, Read, Generic, FromJSON, ToJSON, Ord)
instance ToSchema ExpirationTime
instance ToParamSchema ExpirationTime
deriving instance FromHttpApiData ExpirationTime
deriving instance ToHttpApiData ExpirationTime
instance DBTimestamp ExpirationTime where
  toDbTimestamp (ExpirationTime t) = toLocalTime t
instance ModelTimestamp ExpirationTime where
  fromDbTimestamp = onLocalTime ExpirationTime
