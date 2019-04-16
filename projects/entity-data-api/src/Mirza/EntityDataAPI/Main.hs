{-# LANGUAGE LambdaCase #-}


module Mirza.EntityDataAPI.Main (main) where

import           System.Envy                   (decodeEnv)

import           Options.Applicative

import           Network.HTTP.Client           (newManager)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)

import           Mirza.EntityDataAPI.AuthProxy (runAuthProxy)
import           Mirza.EntityDataAPI.Types
import           Mirza.EntityDataAPI.Utils     (fetchJWKs)

import           Network.HTTP.ReverseProxy     (ProxyDest (..))
import qualified Network.Wai.Handler.Warp      as Warp

import qualified Data.ByteString.Char8         as B

import           Data.String                   (IsString (..))

import           Database.PostgreSQL.Simple    (close, connectPostgreSQL)

import           Data.Pool                     (createPool)

main :: IO ()
-- main = launchProxy =<< execParser opts where
--   opts = info (optsParser <**> helper)
--     (fullDesc
--     <> progDesc "Reverse proxy for Mirza services"
--     <> header "Entity Data API")
main = (decodeEnv :: IO (Either String Opts)) >>= \case
  Left err -> fail $ "Failed to parse Opts: " <> err
  Right opts -> do
    print opts
    launchProxy opts


initContext :: Opts -> IO AuthContext
initContext (Opts myService (ServiceInfo (Hostname destHost) (Port destPort)) url clientId dbConnStr) = do
  let proxyDest = ProxyDest (B.pack destHost) destPort
  mngr <- newManager tlsManagerSettings
  connpool <- createPool (connectPostgreSQL dbConnStr) close
                    1 -- Number of "sub-pools",
                    60 -- How long in seconds to keep a connection open for reuse
                    20 -- Max number of connections to have open at any one time
  fetchJWKs mngr url >>= \case
    Left err -> fail $ show err
    Right jwkSet -> pure $ AuthContext myService proxyDest mngr jwkSet (fromString clientId) connpool

launchProxy :: Opts -> IO ()
launchProxy opts = do
  putStrLn "Initializing context..."
  ctx <- initContext opts
  putStrLn $  "Starting service on " <>
              (getHostname . serviceHost . myProxyServiceInfo $ ctx) <> ":" <>
              (show . servicePort . myProxyServiceInfo $ ctx)
  Warp.run (fromIntegral . getPort . servicePort . myProxyServiceInfo $ ctx) (runAuthProxy ctx)

_optsParser :: Parser Opts
_optsParser = Opts
  <$> (ServiceInfo
        <$> (Hostname <$> strOption (long "host" <> short 'h' <> value "localhost" <> showDefault <> help "The host to run this service on."))
        <*> (Port <$> option auto (long "port" <> short 'p' <> value 8000 <> showDefault <> help "The port to run this service on."))
  )
  <*> (ServiceInfo
        <$> (Hostname <$> strOption (long "desthost" <> short 'd' <> value "localhost" <> showDefault <> help "The host to make requests to."))
        <*> (Port <$> option auto (long "destport" <> short 'r' <> value 8200 <> showDefault <> help "Port to make requests to.")))
  <*> strOption (long "jwkurl" <> short 'j' <> value "https://mirza.au.auth0.com/.well-known/jwks.json" <> showDefault <> help "URL to fetch ")
  <*> strOption (long "jwkclientid" <> short 'k' <> help "Audience Claim.")
  <*> strOption (long "conn" <> short 'c' <> help "Postgresql DB Connection String")
