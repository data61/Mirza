module Mirza.BusinessRegistry.Tests.Client where

import           Mirza.BusinessRegistry.Client.Servant
import           Mirza.BusinessRegistry.Tests.Settings (testDbConnStr)

import           Control.Concurrent                    (ThreadId, forkIO,
                                                        killThread)
import           System.IO.Unsafe                      (unsafePerformIO)

import qualified Network.HTTP.Client                   as C
import           Network.Socket
import qualified Network.Wai                           as Wai
import           Network.Wai.Handler.Warp

import           Servant.API.BasicAuth
import           Servant.Client

import           Data.Text.Encoding                    (encodeUtf8)

import           Test.Tasty.Hspec

import           Mirza.BusinessRegistry.Main           (GlobalOptions (..),
                                                        RunServerOptions (..),
                                                        initApplication,
                                                        initBRContext)
import           Mirza.BusinessRegistry.Types

import           Data.GS1.EPC                          (GS1CompanyPrefix (..))


import           Katip                                 (Severity (DebugS))

-- Cribbed from https://github.com/haskell-servant/servant/blob/master/servant-client/test/Servant/ClientSpec.hs

-- === Servant Client tests

userABC :: NewUser
userABC = NewUser
  { phoneNumber = "0400 111 222"
  , userEmailAddress = EmailAddress "abc@example.com"
  , firstName = "Johnny"
  , lastName = "Smith"
  , company = GS1CompanyPrefix "something"
  , password = "re4lly$ecret14!"}

authABC :: BasicAuthData
authABC = BasicAuthData
  (encodeUtf8 . unEmailAddress . userEmailAddress $ userABC)
  (encodeUtf8 . password                      $ userABC)

runApp :: IO (ThreadId, BaseUrl)
runApp = do
  let go = GlobalOptions testDbConnStr 14 8 1 DebugS Dev
  ctx <- initBRContext go
  startWaiApp =<< initApplication go (RunServerOptions 8000) ctx

clientSpec :: Spec
clientSpec =
  beforeAll runApp $
  afterAll endWaiApp $ do
    {-- FIXME - there is no API function newUser in BR, there probably should be!!
    describe "SupplyChain.Client new user" $ do
    describe "BasicAuth" $ do
        --}
    it "Stub" $ \(_,_baseurl) -> do
      pending

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

runClient :: ClientM a -> BaseUrl -> IO (Either ServantError a)
runClient x baseUrl' = runClientM x (mkClientEnv manager' baseUrl')
