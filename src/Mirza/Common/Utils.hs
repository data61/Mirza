{-# LANGUAGE FlexibleContexts #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Utils
  (
    toText
  , randomText
  , notImplemented
  , newUUID
  ) where


import           Control.Monad          (replicateM)
import           Control.Monad.IO.Class (MonadIO, liftIO)

import qualified Data.Text              as T
import           Data.UUID              (UUID)
import           Data.UUID.V4           (nextRandom)

import           System.Random          (randomRIO)

import           GHC.Stack              (HasCallStack)


-- | Converts anything to a ``Text``
toText :: Show a => a -> T.Text
toText = T.pack . show

randomText :: IO T.Text
randomText = do
  count <- randomRIO (8 :: Int, 32)
  randomString <- (take count) <$> replicateM count (randomRIO ('a', 'z'))
  return $ T.pack randomString

{-# WARNING notImplemented "notImplemented should not be used" #-}
notImplemented :: HasCallStack => a
notImplemented = error "FIXME"

-- | Generate a new v4 UUID - when being used in a database transaction,
-- this should be called inside the DB monad so that if the transaction
-- is retried a new UUID will be generated.
newUUID :: MonadIO m => m UUID
newUUID = liftIO nextRandom
