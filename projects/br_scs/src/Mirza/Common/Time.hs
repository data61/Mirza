{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}


-- | Times cannot be stored as UTC Time by Postgres
-- See this discussion for details:
-- https://groups.google.com/forum/#!topic/beam-discussion/DcC0yik7Pxc
-- However, all times are converted to UTCTime using methods from typeclasses
-- defined in this module
module Mirza.Common.Time
  ( CreationTime(..), RevocationTime(..), ExpirationTime(..)
  , DBTimestamp, ModelTimestamp, fromDbTimestamp, toDbTimestamp
  , generateTimestamp, onLocalTime
  ) where

import           GHC.Generics           (Generic)
import           Servant                (FromHttpApiData (..),
                                         ToHttpApiData (..))

import           Data.Aeson
import           Data.Swagger

import           Data.GS1.EPC           as EPC

import           Data.Time              (LocalTime, UTCTime)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.LocalTime    (localTimeToUTC, utc, utcToLocalTime)

import           Control.Monad.IO.Class (MonadIO, liftIO)

class DBTimestamp t where
  toDbTimestamp :: t -> LocalTime
class ModelTimestamp t where
  fromDbTimestamp :: LocalTime -> t

-- | Reads back the ``LocalTime`` in UTCTime (with an offset of 0)
-- And wraps it in a custom constructor (newtype wrappers around UTCTime)
-- This utility function is primarily used to implement
-- the method(s) of ModelTimestamp
onLocalTime :: (UTCTime -> t) -> LocalTime -> t
onLocalTime c t = c (localTimeToUTC utc t)

toLocalTime :: UTCTime -> LocalTime
toLocalTime = utcToLocalTime utc

-- | Shortcut to generate timestamp in AppM
generateTimestamp :: MonadIO m => m UTCTime
generateTimestamp = liftIO getCurrentTime


instance DBTimestamp UTCTime where
  toDbTimestamp = toLocalTime
instance ModelTimestamp UTCTime where
  fromDbTimestamp = onLocalTime id


instance DBTimestamp EPCISTime where
  toDbTimestamp (EPCISTime t) = toLocalTime t
instance ModelTimestamp EPCISTime where
  fromDbTimestamp = onLocalTime EPCISTime


newtype CreationTime = CreationTime {getCreationTime :: UTCTime}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON, Ord)
instance ToSchema CreationTime
instance ToParamSchema CreationTime
deriving instance FromHttpApiData CreationTime
deriving instance ToHttpApiData CreationTime
instance DBTimestamp CreationTime where
  toDbTimestamp (CreationTime t) = toLocalTime t
instance ModelTimestamp CreationTime where
  fromDbTimestamp = onLocalTime CreationTime


newtype RevocationTime = RevocationTime {getRevocationTime :: UTCTime}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON, Ord)
instance ToSchema RevocationTime
instance ToParamSchema RevocationTime
deriving instance FromHttpApiData RevocationTime
deriving instance ToHttpApiData RevocationTime
instance DBTimestamp RevocationTime where
  toDbTimestamp (RevocationTime t) = toLocalTime t
instance ModelTimestamp RevocationTime where
  fromDbTimestamp = onLocalTime RevocationTime


newtype ExpirationTime = ExpirationTime {getExpirationTime :: UTCTime}
  deriving (Show, Eq, Read, Generic, FromJSON, ToJSON, Ord)
instance ToSchema ExpirationTime
instance ToParamSchema ExpirationTime
deriving instance FromHttpApiData ExpirationTime
deriving instance ToHttpApiData ExpirationTime
instance DBTimestamp ExpirationTime where
  toDbTimestamp (ExpirationTime t) = toLocalTime t
instance ModelTimestamp ExpirationTime where
  fromDbTimestamp = onLocalTime ExpirationTime
