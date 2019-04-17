{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mirza.EntityDataAPI.AuthProxy where

import           Mirza.EntityDataAPI.Database.Utils (doesSubExist)

import           Network.HTTP.ReverseProxy          (WaiProxyResponse (..),
                                                     defaultOnExc, waiProxyTo)
import           Network.HTTP.Types
import           Network.Wai                        (Application, Request (..),
                                                     responseBuilder)

import           GHC.Exception                      (SomeException)

import           Control.Monad.Except               (throwError)
import           Control.Monad.Reader               (asks)

import           Control.Lens                       (view)

import           Mirza.EntityDataAPI.Types
import           Mirza.EntityDataAPI.Utils

import qualified Crypto.JWT                         as JWT

import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as BSL

handleRequest :: AuthContext -> Request -> IO WaiProxyResponse
handleRequest ctx r = do
  let (modifiedReq, mAuthHeader) = extractAuthHeader r
  mUnverifiedJWT <- runAppM ctx $ handleToken mAuthHeader
  case mUnverifiedJWT of
    Left (_err :: AppError) -> pure $ WPRResponse $ responseBuilder status401 mempty mempty
    Right _ -> pure $ WPRModifiedRequest modifiedReq (destProxyServiceInfo ctx)

handleToken :: Maybe Header -> AppM AuthContext AppError JWT.ClaimsSet
handleToken (Just (_, authHdr)) = do
  jwKey <- asks jwtSigningKeys
  aud <- asks ctxJwkClientId
  let bearer = "Bearer "
      (_mbearer, token) = BS.splitAt (BS.length bearer) authHdr
  (unverifiedJWT :: JWT.SignedJWT) <- JWT.decodeCompact $ BSL.fromStrict token
  claimSet <- JWT.verifyClaims (JWT.defaultJWTValidationSettings (== aud)) jwKey unverifiedJWT
  case view JWT.claimSub claimSet of
    Nothing -> throwError NoClaimSubject
    Just sub -> doesSubExist sub >>= \case
        False -> throwError UnauthClaimsSubject
        True  -> pure claimSet
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
