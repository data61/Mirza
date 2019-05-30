{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mirza.EntityDataAPI.Main (main) where

import           System.Envy                        (decodeEnv)

import           Network.HTTP.Client                (newManager)
import           Network.HTTP.Client.TLS            (tlsManagerSettings)

import           Mirza.EntityDataAPI.AuthProxy      (runAuthProxy)
import           Mirza.EntityDataAPI.Database.Utils (addUser, addUserSub)
import           Mirza.EntityDataAPI.Errors
import           Mirza.EntityDataAPI.Types
import           Mirza.EntityDataAPI.Utils          (fetchJWKs)

import           Network.HTTP.ReverseProxy          (ProxyDest (..))
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import           Network.Wai.Middleware.Cors        (CorsResourcePolicy (..),
                                                     cors,
                                                     simpleCorsResourcePolicy,
                                                     simpleMethods)

import qualified Data.ByteString.Char8              as B

import           Data.String                        (IsString (..))

import           Crypto.JWT                         (StringOrURI)

import           Database.PostgreSQL.Simple         (close, connectPostgreSQL)

import           Data.Pool                          (createPool)

import           Data.List.Split                    (splitOn)

import           System.IO                          (hSetBuffering, stdout, BufferMode(LineBuffering))

main :: IO ()
-- main = launchProxy =<< execParser opts where
--   opts = info (optsParser <**> helper)
--     (fullDesc
--     <> progDesc "Reverse proxy for Mirza services"
--     <> header "Entity Data API")
main = (decodeEnv :: IO (Either String Opts)) >>= \case
  Left err -> fail $ "Failed to parse Opts: " <> err
  Right opts -> do
    hSetBuffering stdout LineBuffering
    print opts
    multiplexInitOptions opts

multiplexInitOptions :: Opts -> IO ()
multiplexInitOptions opts = do
  ctx <- initContext opts
  putStrLn $ "Initialized context. Starting app on mode " <> (show . appMode $ opts)
  case appMode opts of
    Proxy     -> launchProxy ctx
    API       -> launchUserManager ctx
    Bootstrap -> do
      res <- tryAddBootstrapUser ctx
      print res

promptLine :: String -> IO String
promptLine prompt = do
  putStr prompt
  getLine


tryAddUser :: AuthContext -> IO (Either AppError ())
tryAddUser ctx = do
  (authorisedUserStr :: String) <- promptLine "Enter thy creds: "
  (toAddUserStr :: String) <- promptLine "User you want to add: "
  let (authorisedUserSub :: StringOrURI) = fromString authorisedUserStr
  let (toAddUserSub :: StringOrURI) = fromString toAddUserStr
  res <- runAppM ctx $ addUserSub authorisedUserSub toAddUserSub
  case res of
    Right () -> putStrLn "Successfully added user"
    Left err -> putStrLn $ "Failed with error : " <> show err
  pure res

tryAddBootstrapUser :: AuthContext -> IO (Either AppError ())
tryAddBootstrapUser ctx = do
  (toAddUserStr :: String) <- promptLine "User you want to add: "
  let (toAddUserSub :: StringOrURI) = fromString toAddUserStr
  res <- runAppM ctx $ addUser toAddUserSub
  case res of
    Right () -> putStrLn "Successfully added user"
    Left err -> putStrLn $ "Failed with error : " <> show err
  pure res


launchUserManager :: AuthContext -> IO ()
launchUserManager ctx = do
  _ <- tryAddUser ctx
  launchUserManager ctx


initContext :: Opts -> IO AuthContext
initContext (Opts myService (ServiceInfo (Hostname destHost) (Port destPort)) _mode url clientIds dbConnStr) = do
  putStrLn "Initializing context..."
  let proxyDest = ProxyDest (B.pack destHost) destPort
  mngr <- newManager tlsManagerSettings
  connpool <- createPool (connectPostgreSQL dbConnStr) close
                    1 -- Number of "sub-pools",
                    60 -- How long in seconds to keep a connection open for reuse
                    20 -- Max number of connections to have open at any one time
  fetchJWKs mngr url >>= \case
    Left err -> fail $ show err
    Right jwkSet -> pure $ AuthContext myService proxyDest mngr jwkSet (parseClientIdList clientIds) connpool
    where
      parseClientIdList cIds = fmap fromString . filter (not . null) . splitOn "," $ cIds


myCors :: Middleware
myCors = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type", "Authorization"]
        , corsMethods = "PUT" : simpleMethods
        , corsOrigins = Just ([
            "http://localhost:8080"
          , "http://localhost:8081"
          , "http://localhost:8000"
          , "https://demo.mirza.d61.io"
          ], True)
        }

launchProxy :: AuthContext -> IO ()
launchProxy ctx = do
  putStrLn $  "Starting service on " <>
              (getHostname . serviceHost . myProxyServiceInfo $ ctx) <> ":" <>
              (show . getPort . servicePort . myProxyServiceInfo $ ctx)
  Warp.run (fromIntegral . getPort . servicePort . myProxyServiceInfo $ ctx) (myCors $ runAuthProxy ctx)

-- _optsParser :: Parser Opts
-- _optsParser = Opts
--   <$> (ServiceInfo
--         <$> (Hostname <$> strOption (long "host" <> short 'h' <> value "localhost" <> showDefault <> help "The host to run this service on."))
--         <*> (Port <$> option auto (long "port" <> short 'p' <> value 8000 <> showDefault <> help "The port to run this service on."))
--   )
--   <*> (ServiceInfo
--         <$> (Hostname <$> strOption (long "desthost" <> short 'd' <> value "localhost" <> showDefault <> help "The host to make requests to."))
--         <*> (Port <$> option auto (long "destport" <> short 'r' <> value 8200 <> showDefault <> help "Port to make requests to.")))
--   <*> (strOption (long "mode" <> short 'm' <> value Proxy <> showDefault <> help "Mode to run the app on. Available modes: Proxy | API"))
--   <*> strOption (long "jwkurl" <> short 'j' <> value "https://mirza.au.auth0.com/.well-known/jwks.json" <> showDefault <> help "URL to fetch ")
--   <*> strOption (long "jwkclientid" <> short 'k' <> help "Audience Claim.")
--   <*> strOption (long "conn" <> short 'c' <> help "Postgresql DB Connection String")
