module Mirza.EntityDataAPI.AuthProxy where

import           Network.HTTP.Types
import           Network.Wai               (Application, Request (..))

import           Network.HTTP.ReverseProxy (ProxyDest (..),
                                            WaiProxyResponse (..), defaultOnExc,
                                            waiProxyTo)

import           GHC.Exception             (SomeException)

import           Mirza.EntityDataAPI.Types
import           Mirza.EntityDataAPI.Utils

handleRequest :: ProxyDest -> Request -> IO WaiProxyResponse
handleRequest pDest r = do
  print r
  let (modifiedReq, mAuthHeader) = extractAuthHeader r
  pure $ WPRModifiedRequest modifiedReq pDest

extractAuthHeader :: Request -> (Request, Maybe Header)
extractAuthHeader r =
  let headers = requestHeaders r
      (restOfHeaders, mAuthHeader) = extract ((==) hAuthorization . fst) headers
  in (r{requestHeaders = restOfHeaders}, mAuthHeader)


handleError :: SomeException -> Application
handleError = defaultOnExc

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

runAuthProxy :: AuthContext -> Application
runAuthProxy context = do
  let proxyDest = destProxyServiceInfo context
  waiProxyTo (handleRequest proxyDest) handleError $ appManager context

