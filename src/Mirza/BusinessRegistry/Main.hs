{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mirza.BusinessRegistry.Main where


import           Mirza.BusinessRegistry.API     (API, ServerAPI, api)
import           Mirza.BusinessRegistry.Auth
import           Mirza.BusinessRegistry.Service
import           Mirza.BusinessRegistry.Types   as BT
import           Mirza.SupplyChain.Migrate      (defConnectionStr, migrate)
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
          <>  short 'i'
          <>  help "Put empty tables into a fresh database"
          )
        <*> option auto
          (
              long "port"
          <>  help "Port to run the service on."
          <>  showDefault
          <>  value defaultPortNumber
          )
        <*> option auto
          (
              long "conn"
          <>  short 'c'
          <>  help "Database connection string."
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
main = launchWithServerOptions =<< execParser opts where
  opts = Options.Applicative.info (serverOptions <**> helper)
    (fullDesc
    <> progDesc "todo some description"
    <> header "Supply Chain Business Registry Server")

launchWithServerOptions :: ServerOptions -> IO ()
--launchWithServerOptions (ServerOptions False _ portNumber databaseConnectionString) = launchServer portNumber databaseConnectionString
launchWithServerOptions options
  | debug(options) = debugFunc
  | otherwise      = runProgram options

-- launchServer :: Int -> ByteString -> IO ()
-- launchServer portNumber databaseConnectionString = do
--     databaseConnection <- connectPostgreSQL databaseConnectionString
--     let
--         env  = Env
--           {
--             envSomeValue = False
--           , envDatabaseConnection = databaseConnection
--           }
--         app = return $ serve apiType (server env)
--     putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/" -- todo replace /swagger-ui/ with a constant...see API module...this is where the constant should be
--     Warp.run (fromIntegral portNumber) =<< app


-- This is a debug function for activating development test stub functions.
-- TODO: Remove this stub before release.
debugFunc :: IO()
debugFunc = do
  putStrLn ("Running Debug Option")
  --databaseConnection <- connectPostgreSQL defaultDatabaseConnectionString
  -- let
  --   env = Env -- todo need to change this for BusinessRegistryEnvironment...just using this to bootstrap.
  --     {
  --       envSomeValue = False
  --     , envDatabaseConnection = databaseConnection
  --     }

  -- databaseDebug env

-- Todo Remove This Function: This function currently servers as the entry point while in the process of splicing in the business registry from the other repository.
runProgram :: ServerOptions -> IO ()
runProgram options
-- FIXME: This is definitely wrong
  | initDatabase(options) = migrate defConnectionStr
  | otherwise  = do
      let portNumber = soPortNumber options
      ctx <- initBRContext options
      app <- initApplication options ctx
      mids <- initMiddleware options
      putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/"
      Warp.run (fromIntegral portNumber) (mids app) `finally` closeScribes (BT._brKatipLogEnv ctx)

initMiddleware :: ServerOptions -> IO Middleware
initMiddleware _ = pure id



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
          (server' ev)

-- easily start the app in ghci, no command line arguments required.
startAppSimple :: ByteString -> IO ()
startAppSimple dbConnStr = do
  let so = ServerOptions Dev False False 8000 dbConnStr 14 8 1 DebugS
  ctx <- initBRContext so
  initApplication so ctx >>= Warp.run 8000


-- Implementation

server' :: BRContext -> Server API
server' ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[BasicAuthCheck BT.AuthUser])
        (appMToHandler ev)
        (appHandlers @BRContext @AppError)
