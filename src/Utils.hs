{-# LANGUAGE OverloadedStrings #-}

-- | General utility functions used throughout the codebase
module Utils where

import qualified Data.Text as T

-- | Converts anything to a ``Text``
toText :: Show a => a -> T.Text
toText = T.pack . show


newtype Bit  = Bit  {unBit :: Int} deriving (Show, Eq, Read)
newtype Byte = Byte {unByte :: Int} deriving (Show, Eq, Read)
