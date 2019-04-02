{-# LANGUAGE DeriveGeneric #-}

module RunApp (run) where

import           Options.Applicative

import           Control.Exception        (finally)

import           AuthProxy                (runAuthProxy)
import           Types

import           GHC.Generics             (Generic)

import           Network.Wai              (Middleware)

import qualified Network.Wai.Handler.Warp as Warp


data ProxyMode
  = AuthProxy
  deriving (Eq, Show, Read, Generic)

data Opts = Opts
  { myServiceInfo   :: ServiceInfo
  , destServiceInfo :: ServiceInfo
  , proxyMode       :: ProxyMode
  }


run :: IO ()
run = multiplexInitOptions =<< execParser opts where
  opts = info (optsParser <**> helper)
    (fullDesc
    <> progDesc "Reverse proxy for Mirza services"
    <> header "Entity Data API")


-- Handles the overriding server options (this effectively defines the point
-- where the single binary could be split into multiple binaries.
multiplexInitOptions :: Opts -> IO ()
multiplexInitOptions (Opts serviceInfo@(myhost, myprt) (desthost, destprt) proxyMode) = case proxyMode of
  AuthProxy -> launchProxy serviceInfo

launchProxy (myhost, myport) = Warp.run (fromIntegral myport) runAuthProxy

optsParser :: Parser Opts
optsParser = Opts
  <$> ((,)
        <$> strOption (long "hostname" <> short 'm' <> value "localhost" <> showDefault <> help "The host to make requests to.")
        <*> option auto (long "port" <> short 'p' <> value 8000 <> showDefault <> help "Port to make requests to.")
  )
  <*> option auto
    ( long "mode" <> short 'm'
    <> value AuthProxy <> showDefault
    <> help "Mode"
    )
