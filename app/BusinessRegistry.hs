{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Mirza.SupplyChain.AppConfig (EnvType (..))
import           Mirza.SupplyChain.Lib
import           Mirza.SupplyChain.Migrate   (defConnectionStr, migrate)

import           Data.ByteString             (ByteString)
import           Data.Semigroup              ((<>))
import           Options.Applicative

import qualified Crypto.Scrypt               as Scrypt


defaultPortNumber :: Int
defaultPortNumber = 8000

defaultDatabaseConnectionString :: ByteString
defaultDatabaseConnectionString = "dbname=devMirzaBusinessRegistry"

data ServerOptions = ServerOptions
  {
      debug                              :: Bool -- TODO: Remove this program option before release.
    , initDatabase                       :: Bool
    , serverOptionsPortNumber            :: Int
    , serverOptionsDatabaseConnectionStr :: ByteString
  }

serverOptions :: Parser ServerOptions
serverOptions = ServerOptions
        <$> switch
          (
             long "debug"
          <> short 'd'
          <> help "Runs the debug command."
          )
        <*> switch
          (
             long "init-db"
          <> short 'i'
          <> help "Put empty tables into a fresh database" )
        <*> option auto
          (
             long "port"
          <> help "Port to run the service on."
          <> showDefault
          <> value defaultPortNumber
          )
        <*> option auto
          (
             long "conn"
          <> short 'c'
          <> help "Database connection string."
          <> showDefault
          <> value defaultDatabaseConnectionString
          )

main :: IO ()
main = launchWithServerOptions =<< execParser opts where
  opts = Options.Applicative.info (serverOptions <**> helper)
    (fullDesc
    <> progDesc "todo some description"
    <> header "Supply Chain Business Registry Server")

launchWithServerOptions :: ServerOptions -> IO ()
launchWithServerOptions (ServerOptions _ True _ _) = debugFunc
--launchWithServerOptions (ServerOptions False _ portNumber databaseConnectionString) = launchServer portNumber databaseConnectionString
launchWithServerOptions options                    = runProgram options

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
runProgram (ServerOptions False _ portNumber databaseConnectionString) = do
  let flavour = Original
  let env = Prod
  startApp databaseConnectionString env (fromIntegral portNumber) flavour Scrypt.defaultParams
runProgram _ = migrate defConnectionStr
