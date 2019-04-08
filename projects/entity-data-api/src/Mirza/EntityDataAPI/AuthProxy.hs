{-# LANGUAGE ScopedTypeVariables #-}

module Mirza.EntityDataAPI.AuthProxy where

import           Network.HTTP.Types
import           Network.Wai               (Application, Request (..))

import           Network.HTTP.ReverseProxy (WaiProxyResponse (..), defaultOnExc,
                                            waiProxyTo)

import           GHC.Exception             (SomeException)

import           Control.Monad.Except      (throwError)
import           Control.Monad.Reader      (asks)

import           Mirza.EntityDataAPI.Types
import           Mirza.EntityDataAPI.Utils

import qualified Crypto.JOSE               as Jose
import qualified Crypto.JWT                as Jose


import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as BSL


handleRequest :: AuthContext -> Request -> IO WaiProxyResponse
handleRequest ctx r = do
  let (modifiedReq, mAuthHeader) = extractAuthHeader r
  mUnverifiedJWT <- runAppM ctx $ handleToken mAuthHeader
  case mUnverifiedJWT of
    Left (err :: AppError) -> fail $ show err
    Right _ -> pure $ WPRModifiedRequest modifiedReq (destProxyServiceInfo ctx)

handleToken :: Maybe Header -> AppM AuthContext AppError Jose.ClaimsSet
handleToken (Just (_, authHdr)) = do
  jwKey <- asks jwtSigningKeys
  let bearer = "Bearer "
      (_mbearer, token) = BS.splitAt (BS.length bearer) authHdr
  unverifiedJWT <- Jose.decodeCompact $ BSL.fromStrict token
  Jose.verifyClaims (Jose.defaultJWTValidationSettings (const True)) jwKey unverifiedJWT
handleToken Nothing = throwError NoAuthHeader

extractAuthHeader :: Request -> (Request, Maybe Header)
extractAuthHeader r =
  let headers = requestHeaders r
      (restOfHeaders, mAuthHeader) = extract ((==) hAuthorization . fst) headers
  in (r{requestHeaders = restOfHeaders}, mAuthHeader)


handleError :: SomeException -> Application
handleError = defaultOnExc

runAuthProxy :: AuthContext -> Application
runAuthProxy context = waiProxyTo (handleRequest context) handleError $ appManager context
