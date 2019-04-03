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
  pure $ WPRModifiedRequest (modifyHeaders r) pDest

modifyHeaders :: Request -> Request
modifyHeaders r =
  let headers = requestHeaders r
      (modifiedHeaders, mAuthHeader) = extract ((==) hAuthorization . fst) headers
  in r{requestHeaders = modifiedHeaders}


handleError :: SomeException -> Application
handleError = defaultOnExc

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

runAuthProxy :: AuthContext -> Application
runAuthProxy context = do
  let proxyDest = destProxyServiceInfo context
  waiProxyTo (handleRequest proxyDest) handleError $ appManager context

