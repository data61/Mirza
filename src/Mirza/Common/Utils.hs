{-# LANGUAGE FlexibleContexts #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Utils
  (
    toText
  , notImplemented
  ) where



import qualified Data.Text as T
import           GHC.Stack (HasCallStack)


-- | Converts anything to a ``Text``
toText :: Show a => a -> T.Text
toText = T.pack . show

{-# WARNING notImplemented "notImplemented should not be used" #-}
notImplemented :: HasCallStack => a
notImplemented = error "FIXME"
