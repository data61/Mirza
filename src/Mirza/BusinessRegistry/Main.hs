{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mirza.BusinessRegistry.Main where


import           Mirza.SupplyChain.API      (API, ServerAPI, api)
import           Mirza.SupplyChain.Auth
import           Mirza.SupplyChain.Migrate  (defConnectionStr, migrate)
import           Mirza.SupplyChain.Service
import           Mirza.SupplyChain.Types    (AppError, EnvType (..),
                                             SCSContext (..), User)

import           Servant                    hiding (header)
import           Servant.Swagger.UI

import qualified Data.Pool                  as Pool
import           Database.PostgreSQL.Simple

import           Network.Wai                (Middleware)
import qualified Network.Wai.Handler.Warp   as Warp

import           Data.ByteString            (ByteString)
import           Data.Semigroup             ((<>))
import           Options.Applicative

import qualified Crypto.Scrypt              as Scrypt


defaultPortNumber :: Int
defaultPortNumber = 8000

defaultDatabaseConnectionString :: ByteString
defaultDatabaseConnectionString = "dbname=devMirzaBusinessRegistry"

data ServerOptions = ServerOptions
  {
      debug                   :: Bool -- TODO: Remove this program option before release.
    , initDatabase            :: Bool
    , soPortNumber            :: Int
    , soDatabaseConnectionStr :: ByteString
    , soScryptN               :: Integer
    , soScryptP               :: Integer
    , soScryptR               :: Integer
  }

serverOptions :: Parser ServerOptions
serverOptions = ServerOptions
        <$> switch
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
      app <- initApplication options
      mids <- initMiddleware options
      putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/"
      Warp.run (fromIntegral portNumber) $ mids app

initMiddleware :: ServerOptions -> IO Middleware
initMiddleware _ = pure id

-- initApplication :: ByteString -> SCSContextType -> ScryptParams -> IO Application
initApplication :: ServerOptions -> IO Application
initApplication (ServerOptions _ _ _ dbConnStr n p r)  = do
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
  let ev  = SCSContext Dev connpool params
      app = serveWithContext api
            (basicAuthServerContext ev)
            (server' ev)
  pure app

-- easily start the app in ghci, no command line arguments required.
startApp_nomain :: ByteString -> IO ()
startApp_nomain dbConnStr =
  initApplication (ServerOptions False False 8000 dbConnStr 14 8 1) >>= Warp.run 8000


-- Implementation

server' :: SCSContext -> Server API
server' ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[BasicAuthCheck User])
        (appMToHandler ev)
        (appHandlers @SCSContext @AppError)
