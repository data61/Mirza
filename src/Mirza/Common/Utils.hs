{-# LANGUAGE FlexibleContexts #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Utils
  (
    toText
  , notImplemented
  , newUUID
  ) where



import qualified Data.Text              as T
import           Data.UUID              (UUID)
import           Data.UUID.V4           (nextRandom)
import           GHC.Stack              (HasCallStack)

import           Control.Monad.IO.Class (MonadIO, liftIO)

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
