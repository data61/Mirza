module AuthProxy (runAuthProxy) where

import           Network.HTTP.Client       (Manager)
import           Network.Wai               (Application, Request)

import           Network.HTTP.ReverseProxy (WaiProxyResponse, defaultOnExc,
                                            waiProxyTo)

import           GHC.Exception             (SomeException)

runAuthProxy :: Request -> IO WaiProxyResponse
runAuthProxy = error "runAuthProxy: not implemented yet"

handleError :: SomeException -> Application
handleError = defaultOnExc
