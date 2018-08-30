module Mirza.Common.Tests.Utils where

import           Data.Time.Clock                        (UTCTime, addUTCTime,
                                                        diffUTCTime,
                                                        getCurrentTime)



-- Checks that the two times are within 1 second of each other.
within1Second :: UTCTime -> UTCTime -> Bool
within1Second expected actual = abs (diffUTCTime expected actual) < (fromInteger 1)


-- Checks that the third time is between the first two (inclusive).
betweenTimes :: UTCTime -> UTCTime -> UTCTime -> Bool
betweenTimes t1 t2 between = (t1 `comparitor` between) && (between `comparitor` t2) where
  comparitor | t1 <= t2  = (<=)
             | otherwise = (>=)