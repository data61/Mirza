module Mirza.Common.Tests.Utils where

import           Data.Time.Clock                        (UTCTime, addUTCTime,
                                                        diffUTCTime,
                                                        getCurrentTime)



-- Checks that the two times are within 1 second of each other.
within1Second :: UTCTime -> UTCTime -> Bool
within1Second expected actual = abs (diffUTCTime expected actual) < (fromInteger 1)

