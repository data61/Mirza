{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mirza.EntityDataAPI.Proxy where

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
import           Data.Maybe                         (listToMaybe)


handleRequest :: EDAPIContext -> Request -> IO WaiProxyResponse
handleRequest ctx r = do
  print $ "Received request: " <> show r
  let (req, mAuthHeader) = extractAuthHeader r
  
  mUnverifiedJWT <- runAppM ctx $ handleToken mAuthHeader
  case mUnverifiedJWT of
    Left _err -> do
      putStrLn ("Token validation failed: " <> show _err)
      pure $ WPRResponse $ responseBuilder status401 mempty mempty
      
    Right _ -> pure $ uncurry WPRModifiedRequest $ handlePath ctx req


handleToken :: Maybe Header -> AppM EDAPIContext AppError JWT.ClaimsSet
handleToken Nothing = throwError NoAuthHeader
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


handlePath :: EDAPIContext -> Request -> (Request, ProxyDest)
handlePath ctx req =
  -- Current, non-breaking approach: 'trails' goes to the trails service, otherwise send it to SCS.
  case pathInfo req of
    ("trails":_) -> (stripHeadingPath req, trailsProxyServiceInfo ctx)
    _            -> (req, scsProxyServiceInfo ctx)
    
  -- Preferred: Seperate endpoints for the SCS and trails service  
  -- case pathInfo req of
  --   ("events":_) -> asks scsProxyServiceInfo
  --   ("trails":_) -> asks trailsProxyServiceInfo
  --   _            -> throwError $ PathErr InvalidPath

  where
    stripHeadingPath request =
      let
        finalPathInfo = tail $ pathInfo request
        finalRawPathInfo = encodeUtf8 $ "/" <> (T.intercalate "/" finalPathInfo)
      in
        request { rawPathInfo = finalRawPathInfo
                , pathInfo = finalPathInfo
                }
  

extractAuthHeader :: Request -> (Request, Maybe Header)
extractAuthHeader originalRequest = (requestWithoutAuthHeader, authHeader) where
  originalHeaders = requestHeaders originalRequest
  (authHeaderList, headersNotAuth) = partition ((== hAuthorization) . fst) originalHeaders
  authHeader = listToMaybe authHeaderList
  requestWithoutAuthHeader = originalRequest{requestHeaders = headersNotAuth}


handleError :: SomeException -> Application
handleError = defaultOnExc

runProxy :: EDAPIContext -> Application
runProxy context = waiProxyTo (handleRequest context) handleError $ appManager context
