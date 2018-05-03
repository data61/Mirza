{-# LANGUAGE FlexibleContexts #-}
-- | General utility functions used throughout the codebase
module Mirza.SupplyChain.Utils where

import qualified Data.Text as T
import           GHC.Stack (HasCallStack)

-- | Converts anything to a ``Text``
toText :: Show a => a -> T.Text
toText = T.pack . show


newtype Bit  = Bit  {unBit :: Int} deriving (Show, Eq, Read)
newtype Byte = Byte {unByte :: Int} deriving (Show, Eq, Read)

{-# WARNING notImplemented "notImplemented should not be used" #-}
notImplemented :: HasCallStack => a
notImplemented = error "FIXME"
