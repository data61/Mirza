module Mirza.Common.Tests.ServantUtils
  ( startWaiApp
  , endWaiApp
  , runClient
  , manager'
  ) where

import           Mirza.Common.Utils       (runClient, manager')

import           Control.Concurrent       (ThreadId, forkIO, killThread)

import           Network.Socket
import qualified Network.Wai              as Wai
import           Network.Wai.Handler.Warp
import           Servant.Client

-- Cribbed from
-- https://github.com/haskell-servant/servant/blob/master/servant-client/test/Servant/ClientSpec.hs
-- License is BSD3, so thank you Zalora South East Asia Pte Ltd, Servant Contributors


startWaiApp :: Wai.Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
    (prt, sock) <- openTestSocket
    let settings = setPort prt defaultSettings
    thread <- forkIO $ runSettingsSocket settings sock app
    pure (thread, BaseUrl Http "localhost" prt "")

endWaiApp :: (ThreadId, BaseUrl) -> IO ()
endWaiApp (thread, _) = killThread thread

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  addr:_ <- getAddrInfo
              (Just $ defaultHints { addrFamily = AF_INET
                                   , addrSocketType = Stream
                                   })
              (Just "127.0.0.1")
              Nothing
  
  s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  bind s (addrAddress addr)
  listen s 1
  prt <- socketPort s
  pure (fromIntegral prt, s)
