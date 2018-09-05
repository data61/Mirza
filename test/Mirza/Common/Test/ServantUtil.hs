module Mirza.Common.Test.ServantUtil
  ( startWaiApp
  , endWaiApp
  , runClient
  , manager'
  , shouldSatisfyIO
  , readRsaPubKey
  , goodRsaPublicKey
  ) where

import           Control.Concurrent           (ThreadId, forkIO,  killThread)
import           System.IO.Unsafe             (unsafePerformIO)

import qualified Network.HTTP.Client          as C
import           Network.Socket
import qualified Network.Wai                  as Wai
import           Network.Wai.Handler.Warp
import           Servant.Client

import           GHC.Stack                    (HasCallStack)
import           Test.Hspec.Expectations      (Expectation, shouldSatisfy)

import           Mirza.BusinessRegistry.Types

import qualified Data.Text.IO                  as TIO

-- Cribbed from
-- https://github.com/haskell-servant/servant/blob/master/servant-client/test/Servant/ClientSpec.hs
-- License is BSD3, so thank you Zalora South East Asia Pte Ltd, Servant Contributors


startWaiApp :: Wai.Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
    (prt, sock) <- openTestSocket
    let settings = setPort prt defaultSettings
    thread <- forkIO $ runSettingsSocket settings sock app
    return (thread, BaseUrl Http "localhost" prt "")

endWaiApp :: (ThreadId, BaseUrl) -> IO ()
endWaiApp (thread, _) = killThread thread

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  prt <- socketPort s
  return (fromIntegral prt, s)

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient :: BaseUrl -> ClientM a -> IO (Either ServantError a)
runClient baseUrl' x = runClientM x (mkClientEnv manager' baseUrl')

shouldSatisfyIO :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyIO` p = action >>= (`shouldSatisfy` p)

-- TODO: This is copied from keys, move it into a higher level module and remove it from here and there after merging with sajid work.
readRsaPubKey :: FilePath -> IO PEM_RSAPubKey
readRsaPubKey filename = PEM_RSAPubKey <$> TIO.readFile filename

-- TODO: this is copied from keys, move it into a higher level module and remove it from here and there after merging with sajid work.
goodRsaPublicKey :: IO PEM_RSAPubKey
goodRsaPublicKey = readRsaPubKey "./test/Mirza/Common/testKeys/goodKeys/4096bit_rsa_key.pub"
