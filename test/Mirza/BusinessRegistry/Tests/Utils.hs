module Mirza.BusinessRegistry.Tests.Utils where

import qualified Data.Text.IO                           as TIO

import           Mirza.BusinessRegistry.Types


-- Read an PEM RSA key from file.
readRsaPubKey :: FilePath -> IO PEM_RSAPubKey
readRsaPubKey filename = PEM_RSAPubKey <$> TIO.readFile filename

-- Gets a good PEM RSA key from file to use from test cases.
goodRsaPublicKey :: IO PEM_RSAPubKey
goodRsaPublicKey = readRsaPubKey "./test/Mirza/Common/TestData/testKeys/goodKeys/4096bit_rsa_key.pub"

-- | Converts from number of seconds to the number of microseconds.
secondsToMicroseconds :: (Num a) => a -> a
secondsToMicroseconds = (* 1000000)