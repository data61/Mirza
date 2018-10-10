{-# LANGUAGE FlexibleContexts #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Tests.Utils where

import           Data.Maybe              (fromJust)

import           Data.ByteString         as BS

import           Text.Email.Validate     (EmailAddress, emailAddress)

import           Test.Hspec.Expectations (Expectation, shouldSatisfy)

import           Data.Time.Clock         (UTCTime, diffUTCTime)

import           GHC.Stack               (HasCallStack)


--------------------------------------------------------------------------------
-- Generic Predicate Utils
--------------------------------------------------------------------------------

-- Checks that the two times are within 1 second of each other.
within1Second :: UTCTime -> UTCTime -> Bool
within1Second expected actual = abs (diffUTCTime expected actual) < 1 -- second


-- | Checks whether a value is between two bounds.
-- | The comparision is done inclusive of the two bounds.
betweenInclusive :: Ord a =>
                       a     -- ^ One of the bounds to check
                    -> a     -- ^ The other bound to check.
                    -> a     -- ^ The value to check if it exists between the two bounds.
                    -> Bool  -- ^ Whether the value is between the two bounds inclusive of the bounds.
betweenInclusive bound1 bound2 x = (bound1 `comparitor` x) && (x `comparitor` bound2) where
  comparitor | bound1 <= bound2  = (<=)
             | otherwise         = (>=)


--------------------------------------------------------------------------------
-- Hspec Utils
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Email Utils
--------------------------------------------------------------------------------
shouldSatisfyIO :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyIO` p = action >>= (`shouldSatisfy` p)

-- | Only use this with hardcodes email addresses that are guaranteed to return
-- a ``Just``
unsafeMkEmailAddress :: BS.ByteString -> EmailAddress
unsafeMkEmailAddress = fromJust . emailAddress
