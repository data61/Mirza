{-# LANGUAGE FlexibleContexts #-}
-- | General utility functions used throughout the codebase
module Mirza.SupplyChain.Utils
  (
    toText
  , notImplemented
  ) where

import qualified Data.Text          as T
import           GHC.Stack          (HasCallStack)
import           Mirza.Common.Types (notImplemented)

-- | Converts anything to a ``Text``
toText :: Show a => a -> T.Text
toText = T.pack . show


