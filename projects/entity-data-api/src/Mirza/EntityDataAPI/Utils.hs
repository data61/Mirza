{-# LANGUAGE ScopedTypeVariables #-}

module Mirza.EntityDataAPI.Utils where

import           Mirza.EntityDataAPI.Errors (AppError (..))

import           Network.HTTP.Client        ( Manager)
import           Network.HTTP.Req           

import           Crypto.JOSE                (JWKSet)

import           Control.Lens

import           Control.Monad              ((<$!>))

import           Control.Exception          (try)

import           Data.Aeson                 (Result (..), Value (..), fromJSON)
import           Data.Aeson.Lens

import           Data.Default
  
import           Data.Text                  (Text)
import           Data.Text.Encoding
import           Data.Text.Strict.Lens      (packed, utf8)

import           Data.ByteString.Base16     as B16
import           Data.ByteString.Base64.URL as B64


-- | Extract one element from a list.
-- Function implementation copied from:
-- https://hackage.haskell.org/package/raft-0.3.7.0/docs/Data-List-Util.html
extract :: (a -> Bool) -> [a] -> ([a], Maybe a)
extract _ [] = ([], Nothing)
extract p x =
  let
    extract' a @ (_, _, Just _) = a
    extract' a @ (_, [], Nothing) = a
    extract' (y', ze : zs, Nothing)
      | p ze = (y', zs, Just ze)
      | otherwise = extract' (ze : y', zs, Nothing)
    (y, z, w) = extract' ([], x, Nothing)
  in
    (rollback y z, w)


-- | Reverse a first list and add it to a second one.
rollback ::
      [a] -- ^ The list to be reversed and prepended.
   -> [a] -- ^ The list to be appended.
   -> [a] -- ^ The resulting list
rollback = flip (foldl (flip (:)))


fetchJWKs :: Manager -> String -> IO (Either AppError JWKSet)
fetchJWKs m url =
  case parseUrlHttps (url ^. packed . re utf8) of
    Nothing   -> pure $ Left UrlParseFailed
    Just url' -> ((toJWKS =<<) . (fmap responseBody)) <$!> (mkReq url')
    where
      mkReq url' = do
        res <- try $ runReq (def { httpConfigAltManager = Just m }) $ req GET (url' ^. _1) NoReqBody jsonResponse mempty
        -- TODO:             ^ def will change to defaultHttpConfig once req-2.0 is in stackage LTS.
        case res of
          Left (e :: HttpException) -> pure . Left  $ ReqFailure e
          Right v                   -> pure . Right $ v

      -- Change the x5t to be spec compliant
      toJWKS :: Value -> Either AppError JWKSet
      toJWKS v = let v' = over (key "keys" . values . key "x5t" . _String) b64HexToB64 v in
        case fromJSON v' of
          Error e   -> Left $ JWKParseFailure e
          Success a -> Right a
      b64HexToB64 :: Text -> Text
      b64HexToB64 = decodeUtf8 . B64.encode . fst . B16.decode . B64.decodeLenient . encodeUtf8
