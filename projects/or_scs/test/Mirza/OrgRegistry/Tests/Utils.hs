module Mirza.OrgRegistry.Tests.Utils where

import           Mirza.Common.Utils (readJWK)

import           Crypto.JOSE (JWK)

-- Gets a good PEM RSA key from file to use from test cases.
goodRsaPublicKey :: IO (Maybe JWK)
goodRsaPublicKey = readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/4096bit_rsa_pub.json"

-- Gets a good PEM RSA private key from file to use from test cases.
goodRsaPrivateKey :: IO (Maybe JWK)
goodRsaPrivateKey = readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/4096bit_rsa.json"

-- | Converts from number of seconds to the number of microseconds.
secondsToMicroseconds :: (Num a) => a -> a
secondsToMicroseconds = (* 1000000)
