{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mirza.BusinessRegistry.Main where


import           Mirza.BusinessRegistry.API     (API, ServerAPI, api)
import           Mirza.BusinessRegistry.Auth
import           Mirza.BusinessRegistry.Service
import           Mirza.BusinessRegistry.Types   as BT
import           Mirza.SupplyChain.Migrate      (migrate)
import           Mirza.SupplyChain.Types        (AppError, EnvType (..))

import           Servant                        hiding (header)
import           Servant.Swagger.UI

import qualified Data.Pool                      as Pool
import           Database.PostgreSQL.Simple

import           Network.Wai                    (Middleware)
import qualified Network.Wai.Handler.Warp       as Warp

import           Data.ByteString                (ByteString)
import           Data.Semigroup                 ((<>))
import           Data.Text                      (pack)
import           Options.Applicative

import qualified Crypto.Scrypt                  as Scrypt

import           Control.Exception              (finally)
import           Katip                          as K
import           System.IO                      (stdout)



defaultPortNumber :: Int
defaultPortNumber = 8000

defaultDatabaseConnectionString :: ByteString
defaultDatabaseConnectionString = "dbname=devMirzaBusinessRegistry"

data ServerOptions = ServerOptions
  { soEnvType                 :: EnvType
    , debug                   :: Bool -- TODO: Remove this program option before release.
    , initDatabase            :: Bool
    , soPortNumber            :: Int
    , soDatabaseConnectionStr :: ByteString
    , soScryptN               :: Integer
    , soScryptP               :: Integer
    , soScryptR               :: Integer
    , soLoggingLevel          :: K.Severity

  }

serverOptions :: Parser ServerOptions
serverOptions = ServerOptions
        <$> option auto
          ( long "env" <> short 'e'
          <> value Dev <> showDefault
          <> help "Environment, Dev | Prod"
          )
        <*> switch
          (
              long "debug"
          <>  short 'd'
          <>  help "Runs the debug command."
          )
        <*> switch
          (
              long "init-db"
          <>  help "Put empty tables into a fresh database"
          )
        <*> option auto
          (
              long "port"
          <>  help "Port to run the service on."
          <>  showDefault
          <>  value defaultPortNumber
          )
        <*> strOption
          (
              long "conn"
          <>  short 'c'
          <>  help "Database connection string in libpq format. See: https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
          <>  showDefault
          <>  value defaultDatabaseConnectionString
          )
        <*> option auto
          (
              long "scryptN"
          <>  help "Scrypt N parameter (>= 14)"
          <>  showDefault
          <>  value 14
          )
        <*> option auto
          (
              long "scryptP"
          <>  help "Scrypt r parameter (>= 8)"
          <>  showDefault
          <>  value 8
          )
        <*> option auto
          (
              long "scryptR"
          <>  help "Scrypt r parameter (>= 1)"
          <> showDefault
          <> value 1
          )
       <*> option auto
          (  long "log-level"
          <> value InfoS
          <> showDefault
          <> help ("Logging level: " ++ show [minBound .. maxBound :: Severity])
          )



main :: IO ()
main = multiplexGlobalOptions =<< execParser opts where
  opts = Options.Applicative.info (serverOptions <**> helper)
    (fullDesc
    <> progDesc "todo some description"
    <> header "Supply Chain Business Registry Service")


-- Handles the overriding global options (this effectively defines the point
-- where the single binary could be split into multiple binaries.
multiplexGlobalOptions :: ServerOptions -> IO ()
multiplexGlobalOptions options
  | debug(options)        = debugFunc
  | initDatabase(options) = migrate $ soDatabaseConnectionStr options
  | otherwise             = launchServer options


launchServer :: ServerOptions -> IO ()
launchServer options = do
      let portNumber = soPortNumber options
      ctx <- initBRContext options
      app <- initApplication options ctx
      mids <- initMiddleware options
      putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/"
      Warp.run (fromIntegral portNumber) (mids app) `finally` closeScribes (BT._brKatipLogEnv ctx)


initBRContext :: ServerOptions -> IO BT.BRContext
initBRContext (ServerOptions envT _ _ _ dbConnStr n p r lev) = do
  handleScribe <- mkHandleScribe ColorIfTerminal stdout lev V3
  logEnv <- initLogEnv "businessRegistry" (Environment . pack . show $ envT)
            >>= registerScribe "stdout" handleScribe defaultScribeSettings
  params <- case Scrypt.scryptParams (max n 14) (max p 8) (max r 1) of
    Just scparams -> pure scparams
    Nothing -> do
      putStrLn $ "Invalid Scrypt params:" ++ show (n,p,r) ++ " using defaults"
      pure Scrypt.defaultParams
  connpool <- Pool.createPool (connectPostgreSQL dbConnStr) close
                      1 -- Number of "sub-pools",
                      60 -- How long in seconds to keep a connection open for reuse
                      10 -- Max number of connections to have open at any one time
                      -- TODO: Make this a config paramete
  pure $ BRContext envT connpool params logEnv mempty mempty


initApplication :: ServerOptions -> BT.BRContext -> IO Application
initApplication _so ev =
  pure $ serveWithContext api
          (basicAuthServerContext ev)
          (server ev)


initMiddleware :: ServerOptions -> IO Middleware
initMiddleware _ = pure id


-- Implementation
server :: BRContext -> Server API
server ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[BasicAuthCheck BT.AuthUser])
        (appMToHandler ev)
        (appHandlers @BRContext @AppError)



-- This is a debug function for activating development test stub functions.
-- TODO: Remove this stub before release.
debugFunc :: IO()
debugFunc = do
  putStrLn ("Running Debug Option")
  -- Debug test code goes here...
