{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mirza.EntityDataAPI.AuthProxy where

import           Mirza.EntityDataAPI.Database.Utils (doesSubExist)

import           Network.HTTP.ReverseProxy          (ProxyDest (..),
                                                     WaiProxyResponse (..),
                                                     defaultOnExc, waiProxyTo)
import           Network.HTTP.Types
import           Network.Wai                        (Application, Request (..),
                                                     responseBuilder)

import           GHC.Exception                      (SomeException)

import           Control.Monad.Except               (throwError)
import           Control.Monad.Reader               (asks, liftIO)

import           Control.Lens                       (view)

import           Mirza.EntityDataAPI.Errors
import           Mirza.EntityDataAPI.Types

import qualified Crypto.JWT                         as JWT

import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Lazy               as BSL
import qualified Data.Text                          as T
import           Data.Text.Encoding                 (encodeUtf8)

import           Data.List                          (partition)

import           Data.Either                        (isLeft)
import           Data.Maybe                         (listToMaybe)


handleRequest :: EDAPIContext -> Request -> IO WaiProxyResponse
handleRequest ctx r = do
  liftIO . print $ "Received request: " <> show r
  let (modifiedReq, mAuthHeader) = extractAuthHeader r
  destServiceOrError <- runAppM ctx $ handlePath r
  if (isLeft destServiceOrError) then pure $ WPRResponse $ responseBuilder status400 mempty mempty
  else do
    let (Right destService) = destServiceOrError
    mUnverifiedJWT <- runAppM ctx $ handleToken mAuthHeader
    case mUnverifiedJWT of
      Left (_err :: AppError) -> do
        liftIO $ putStrLn ("Token validation failed: " <> show _err)
        pure $ WPRResponse $ responseBuilder status401 mempty mempty
      Right _ -> do
        let strippedReq = stripHeadingPath modifiedReq
        pure $ WPRModifiedRequest strippedReq destService

handleToken :: Maybe Header -> AppM EDAPIContext AppError JWT.ClaimsSet
handleToken (Just (_, authHdr)) = do
  jwKey <- asks jwtSigningKeys
  aud <- asks ctxJwkClientIds
  let bearer = "Bearer "
      (_mbearer, token) = BS.splitAt (BS.length bearer) authHdr
  (unverifiedJWT :: JWT.SignedJWT) <- JWT.decodeCompact $ BSL.fromStrict token
  claimSet <- JWT.verifyClaims (JWT.defaultJWTValidationSettings (`elem` aud)) jwKey unverifiedJWT
  case view JWT.claimSub claimSet of
    Nothing -> throwError NoClaimSubject
    Just sub -> doesSubExist sub >>= \case
        False -> throwError UnauthClaimsSubject
        True  -> do
          liftIO . putStrLn $ "\nAUTHENTICATED\n"
          pure claimSet
handleToken Nothing = throwError NoAuthHeader

stripHeadingPath :: Request -> Request
stripHeadingPath req = do
  let finalPathInfo = tail $ pathInfo req
      finalRawPathInfo = encodeUtf8 $ "/" <> (T.intercalate "/" finalPathInfo)
  req{rawPathInfo=finalRawPathInfo, pathInfo=finalPathInfo}

handlePath :: Request -> AppM EDAPIContext AppError ProxyDest
handlePath req = do
  let paths = pathInfo req
  case paths of
    ("events":_) -> asks scsProxyServiceInfo
    ("trails":_) -> asks trailsProxyServiceInfo
    _            -> throwError $ PathErr InvalidPath

extractAuthHeader :: Request -> (Request, Maybe Header)
extractAuthHeader originalRequest = (requestWithoutAuthHeader, authHeader) where
  originalHeaders = requestHeaders originalRequest
  (authHeaderList, headersNotAuth) = partition ((== hAuthorization) . fst) originalHeaders
  authHeader = listToMaybe authHeaderList
  requestWithoutAuthHeader = originalRequest{requestHeaders = headersNotAuth}


handleError :: SomeException -> Application
handleError = defaultOnExc


runAuthProxy :: EDAPIContext -> Application
runAuthProxy context = waiProxyTo (handleRequest context) handleError $ appManager context
