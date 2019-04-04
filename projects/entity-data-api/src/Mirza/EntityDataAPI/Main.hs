module Mirza.EntityDataAPI.Main (main) where

import           Options.Applicative

import           Network.HTTP.Client           (newManager)
import           Network.HTTP.Client.TLS       (tlsManagerSettings)

import           Mirza.EntityDataAPI.AuthProxy (runAuthProxy)
import           Mirza.EntityDataAPI.Types

import           Network.HTTP.ReverseProxy     (ProxyDest (..))
import qualified Network.Wai.Handler.Warp      as Warp

import qualified Data.ByteString.Char8         as B

import           Crypto.JOSE.JWK               (KeyMaterialGenParam (..),
                                                genJWK)

data Opts = Opts
  { _myServiceInfo   :: ServiceInfo
  , _destServiceInfo :: ServiceInfo
  , _keySize         :: Int
  }

main :: IO ()
main = launchProxy =<< execParser opts where
  opts = info (optsParser <**> helper)
    (fullDesc
    <> progDesc "Reverse proxy for Mirza services"
    <> header "Entity Data API")

initContext :: Opts -> IO AuthContext
initContext (Opts myService (destHost, destPort) kSize) = do
  mngr <- newManager tlsManagerSettings
  jwKey <- genJWK (RSAGenParam kSize)
  let proxyDest = ProxyDest (B.pack destHost) destPort
  pure $ AuthContext myService proxyDest mngr jwKey

launchProxy :: Opts -> IO ()
launchProxy opts = do
  putStrLn "Initializing context..."
  ctx <- initContext opts
  putStrLn $ "Starting service on " <> (fst . myProxyServiceInfo $ ctx) <> ":" <> (show . snd . myProxyServiceInfo $ ctx)
  Warp.run (fromIntegral . snd . myProxyServiceInfo $ ctx) (runAuthProxy ctx)

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
  <*> option auto (long "keysize" <> short 'k' <> value 256 <> showDefault <> help "RSA Key size (Bytes)")
