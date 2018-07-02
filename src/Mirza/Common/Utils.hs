{-# LANGUAGE FlexibleContexts #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Utils
  (
    toText
  , notImplemented
  , newUUID
  , onLocalTime
  , toZonedTime
  , toLocalTime
  , generateTimestamp
  ) where



import qualified Data.Text              as T
import           Data.UUID              (UUID)
import           Data.UUID.V4           (nextRandom)
import           GHC.Stack              (HasCallStack)

import           Control.Monad.IO.Class (MonadIO, liftIO)

import           Data.Time              (UTCTime, ZonedTime (..),
                                         utcToZonedTime)
import           Data.Time.Clock        (getCurrentTime)
import           Data.Time.LocalTime    (LocalTime, localTimeToUTC, utc,
                                         utcToLocalTime)

-- | Converts anything to a ``Text``
toText :: Show a => a -> T.Text
toText = T.pack . show

{-# WARNING notImplemented "notImplemented should not be used" #-}
notImplemented :: HasCallStack => a
notImplemented = error "FIXME"


-- | Generate a new v4 UUID - when being used in a database transaction,
-- this should be called inside the DB monad so that if the transaction
-- is retried a new UUID will be generated.
newUUID :: MonadIO m => m UUID
newUUID = liftIO nextRandom



-- | Reads back the ``LocalTime`` in UTCTime (with an offset of 0)
-- And wraps it in a custom constructor (newtype wrappers around UTCTime)
onLocalTime :: (UTCTime -> t) -> LocalTime -> t
onLocalTime c t = c (localTimeToUTC utc t)

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
toZonedTime :: UTCTime -> ZonedTime
toZonedTime = utcToZonedTime utc

toLocalTime :: UTCTime -> LocalTime
toLocalTime = utcToLocalTime utc

-- | Generates a timestamp in LocalTime + 0:00 offset
-- which is a UTCTime
generateTimestamp :: MonadIO m => m LocalTime
generateTimestamp = utcToLocalTime utc <$> liftIO getCurrentTime
