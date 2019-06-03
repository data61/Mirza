{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mirza.Common.Utils where

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


data FetchJWKsError =
    UrlParseFailed
  | ReqFailure HttpException
  | JWKParseFailure [Char]
  deriving Show

fetchJWKs :: Manager -> String -> IO (Either FetchJWKsError JWKSet)
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
      toJWKS :: Value -> Either FetchJWKsError JWKSet
      toJWKS v = let v' = over (key "keys" . values . key "x5t" . _String) b64HexToB64 v in
        case fromJSON v' of
          Error e   -> Left $ JWKParseFailure e
          Success a -> Right a
      b64HexToB64 :: Text -> Text
      b64HexToB64 = decodeUtf8 . B64.encode . fst . B16.decode . B64.decodeLenient . encodeUtf8
