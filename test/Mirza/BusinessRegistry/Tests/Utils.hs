module Mirza.BusinessRegistry.Tests.Utils where

import qualified Data.Text.IO                 as TIO

import           Mirza.BusinessRegistry.Types

import           OpenSSL.EVP.PKey             (SomeKeyPair)
import           OpenSSL.PEM

-- Read an PEM RSA key from file.
readRsaPublicKey :: FilePath -> IO PEM_RSAPubKey
readRsaPublicKey filename = PEM_RSAPubKey <$> TIO.readFile filename

-- Gets a PEM RSA key from file to use from test cases.
readRsaPrivateKey :: FilePath -> IO SomeKeyPair
readRsaPrivateKey fileName = do
  privKeyStr <- readFile fileName
  readPrivateKey privKeyStr PwNone

-- Gets a good PEM RSA key from file to use from test cases.
goodRsaPublicKey :: IO PEM_RSAPubKey
goodRsaPublicKey = readRsaPublicKey "./test/Mirza/Common/TestData/testKeys/goodKeys/4096bit_rsa_key.pub"

-- Gets a good PEM RSA private key from file to use from test cases.
goodRsaPrivateKey :: IO SomeKeyPair
goodRsaPrivateKey = readRsaPrivateKey "./test/Mirza/Common/TestData/testKeys/goodKeys/4096bit_rsa_key.key"

-- | Converts from number of seconds to the number of microseconds.
secondsToMicroseconds :: (Num a) => a -> a
secondsToMicroseconds = (* 1000000)
