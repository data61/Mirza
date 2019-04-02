module AuthProxy (runAuthProxy) where

import           Network.HTTP.Client       (Manager, defaultManagerSettings,
                                            newManager)
import           Network.Wai               (Application, Request)

import           Network.HTTP.ReverseProxy (WaiProxyResponse, defaultOnExc,
                                            waiProxyTo)

import           GHC.Exception             (SomeException)

import           System.IO.Unsafe          (unsafePerformIO)

handleRequest :: Request -> IO WaiProxyResponse
handleRequest = error "runAuthProxy: not implemented yet"

handleError :: SomeException -> Application
handleError = defaultOnExc

{-# NOINLINE runAuthProxy #-}
runAuthProxy :: Application
runAuthProxy = waiProxyTo handleRequest handleError mngr
  where
    mngr = unsafePerformIO $ newManager defaultManagerSettings
