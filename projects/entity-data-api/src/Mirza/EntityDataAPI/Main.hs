{-# LANGUAGE DeriveGeneric #-}

module Mirza.EntityDataAPI.Main (main) where

import           Options.Applicative

import           Network.HTTP.Client           (Manager, defaultManagerSettings,
                                                newManager)

import           Control.Exception             (finally)

import           Mirza.EntityDataAPI.AuthProxy (runAuthProxy)
import           Mirza.EntityDataAPI.Types

import           GHC.Generics                  (Generic)

import           Network.Wai                   (Middleware)

import           Network.HTTP.ReverseProxy     (ProxyDest (..))
import qualified Network.Wai.Handler.Warp      as Warp

import qualified Data.ByteString.Char8         as B

data ProxyMode
  = AuthProxy
  deriving (Eq, Show, Read, Generic)

data Opts = Opts
  { myServiceInfo   :: ServiceInfo
  , destServiceInfo :: ServiceInfo
  , proxyMode       :: ProxyMode
  }


main :: IO ()
main = multiplexInitOptions =<< execParser opts where
  opts = info (optsParser <**> helper)
    (fullDesc
    <> progDesc "Reverse proxy for Mirza services"
    <> header "Entity Data API")

initContext :: Opts -> IO AuthContext
initContext (Opts myService (destHost, destPort) _) = do
  mngr <- newManager defaultManagerSettings
  let proxyDest = ProxyDest (B.pack destHost) destPort
  pure $ AuthContext myService proxyDest mngr

-- Handles the overriding server options (this effectively defines the point
-- where the single binary could be split into multiple binaries.
multiplexInitOptions :: Opts -> IO ()
multiplexInitOptions (Opts serviceInfo@(myhost, myprt) (desthost, destprt) proxyMode) = case proxyMode of
  AuthProxy -> launchProxy serviceInfo

launchProxy (myhost, myport) = Warp.run (fromIntegral myport) runAuthProxy

optsParser :: Parser Opts
optsParser = Opts
  <$> ((,)
        <$> strOption (long "host" <> short 'h' <> value "localhost" <> showDefault <> help "The host to run this service on.")
        <*> option auto (long "port" <> short 'p' <> value 8000 <> showDefault <> help "The port to run this service on.")
  )
  <*> ((,)
        <$> strOption (long "desthost" <> short 'd' <> value "localhost" <> showDefault <> help "The host to make requests to.")
        <*> option auto (long "destport" <> short 'r' <> value 8200 <> showDefault <> help "Port to make requests to.")
  )
  <*> option auto
    ( long "mode" <> short 'm'
    <> value AuthProxy <> showDefault
    <> help "Mode"
    )
