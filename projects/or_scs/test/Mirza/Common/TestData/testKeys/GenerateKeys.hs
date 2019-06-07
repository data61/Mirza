#!/usr/bin/env stack
-- stack --resolver lts-12.12 script --package jose,aeson-pretty,cryptonite,lens,bytestring

-- Used to generate good and bad keys for use with the tests. To run just
-- execute
--    ./GenerateKeys.hs
-- The file should be marked executable, and uses stack's `script` support.

module Main where

import           Crypto.JOSE
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy.Char8 as BS

import           Control.Lens               ((^.))

writeJWKToFile :: FilePath -> JWK -> IO ()
writeJWKToFile path key = do
  let fullPath = (path <> ".json")
      pretty = (encodePretty key)
  BS.putStrLn . pack $ "\n# Writing to: " ++ fullPath
  BS.putStrLn pretty
  BS.writeFile fullPath pretty

writeKeyPair :: FilePath -> JWK -> IO ()
writeKeyPair path key = do
  writeJWKToFile path key
  let Just pub = key ^. asPublicKey
  writeJWKToFile (path <> "_pub") pub

-- | generate an RSA key with the given number of bits
makeRSA :: Int -> IO JWK
makeRSA size = genJWK (RSAGenParam (size `div` 8))

main :: IO ()
main = do
  writeKeyPair "goodJWKs/2048bit_rsa" =<< makeRSA 2048
  writeKeyPair "goodJWKs/4096bit_rsa" =<< makeRSA 4096
  writeKeyPair "goodJWKs/16384bit_rsa" =<< makeRSA 16384

  writeKeyPair "badJWKs/1024bit_rsa" =<< makeRSA 1024
  writeKeyPair "badJWKs/2040bit_rsa" =<< makeRSA 2040

