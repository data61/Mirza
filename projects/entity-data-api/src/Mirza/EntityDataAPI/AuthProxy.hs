module Mirza.EntityDataAPI.AuthProxy (runAuthProxy) where

import           Network.Wai               (Application, Request (..))

import           Network.HTTP.ReverseProxy (ProxyDest (..),
                                            WaiProxyResponse (..), defaultOnExc,
                                            waiProxyTo)

import           GHC.Exception             (SomeException)

import           Mirza.EntityDataAPI.Types

handleRequest :: ProxyDest -> Request -> IO WaiProxyResponse
handleRequest pDest r = do
  print r
  pure $ WPRModifiedRequest r{requestHeaders = []} pDest

handleError :: SomeException -> Application
handleError = defaultOnExc

-- type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

{-# NOINLINE runAuthProxy #-}
runAuthProxy :: AuthContext -> Application
runAuthProxy context = do
  let proxyDest = destProxyServiceInfo context
  waiProxyTo (handleRequest proxyDest) handleError $ appManager context

