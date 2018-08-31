module Mirza.Common.Tests.Utils where

import           Data.Time.Clock                        (UTCTime, addUTCTime,
                                                        diffUTCTime,
                                                        getCurrentTime)



-- Checks that the two times are within 1 second of each other.
within1Second :: UTCTime -> UTCTime -> Bool
within1Second expected actual = abs (diffUTCTime expected actual) < (fromInteger 1)


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
