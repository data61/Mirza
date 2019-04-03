module Mirza.EntityDataAPI.AuthProxy (runAuthProxy, stripAuthHeader) where

import           Network.HTTP.Types
import           Network.Wai               (Application, Request (..))

import           Network.HTTP.ReverseProxy (ProxyDest (..),
                                            WaiProxyResponse (..), defaultOnExc,
                                            waiProxyTo)

import           GHC.Exception             (SomeException)

import           Mirza.EntityDataAPI.Types

handleRequest :: ProxyDest -> Request -> IO WaiProxyResponse
handleRequest pDest r = do
  print r
  pure $ WPRModifiedRequest (modifyHeaders r) pDest

modifyHeaders :: Request -> Request
modifyHeaders r =
  let headers = requestHeaders r
      modifiedHeaders = stripAuthHeader headers
  in r{requestHeaders = modifiedHeaders}

stripAuthHeader :: RequestHeaders -> RequestHeaders
stripAuthHeader (x@(hName, _):xs) = if hName == hAuthorization then xs else x : stripAuthHeader xs
stripAuthHeader []                = []

handleError :: SomeException -> Application
handleError = defaultOnExc

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

runAuthProxy :: AuthContext -> Application
runAuthProxy context = do
  let proxyDest = destProxyServiceInfo context
  waiProxyTo (handleRequest proxyDest) handleError $ appManager context

