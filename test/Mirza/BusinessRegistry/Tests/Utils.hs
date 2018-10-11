module Mirza.BusinessRegistry.Tests.Utils where

import           Crypto.JOSE (JWK)
import           Data.Aeson  (decodeFileStrict)

-- Read an PEM RSA key from file.
readRsaPublicKey :: FilePath -> IO (Maybe JWK)
readRsaPublicKey = decodeFileStrict

-- Gets a PEM RSA key from file to use from test cases.
readRsaPrivateKey :: FilePath -> IO (Maybe JWK)
readRsaPrivateKey = decodeFileStrict

-- Gets a good PEM RSA key from file to use from test cases.
goodRsaPublicKey :: IO (Maybe JWK)
goodRsaPublicKey = readRsaPublicKey "./test/Mirza/Common/TestData/testKeys/goodKeys/4096bit_rsa_key.pub"

-- Gets a good PEM RSA private key from file to use from test cases.
goodRsaPrivateKey :: IO (Maybe JWK)
goodRsaPrivateKey = readRsaPrivateKey "./test/Mirza/Common/TestData/testKeys/goodKeys/4096bit_rsa_key.key"

-- | Converts from number of seconds to the number of microseconds.
secondsToMicroseconds :: (Num a) => a -> a
secondsToMicroseconds = (* 1000000)
