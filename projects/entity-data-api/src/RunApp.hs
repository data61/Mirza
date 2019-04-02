{-# LANGUAGE DeriveGeneric #-}

module RunApp (run) where

import           Options.Applicative

import           AuthProxy           (runAuthProxy)
import           Types

import           GHC.Generics        (Generic)

data ProxyMode
  = AuthProxy
  deriving (Eq, Show, Read, Generic)


data Opts = Opts
  { serviceInfo :: (String, Int) -- (Host, Port)
  , proxyMode   :: ProxyMode
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
multiplexInitOptions (Opts (hostname, prt) proxyMode) = case proxyMode of
  AuthProxy -> runAuthProxy


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