{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Mirza.BusinessRegistry.Main where


import           Mirza.BusinessRegistry.API             (API, ServerAPI, api)
import           Mirza.BusinessRegistry.Auth
import           Mirza.BusinessRegistry.Database.Schema as Schema
import           Mirza.BusinessRegistry.Service
import           Mirza.BusinessRegistry.Types           as BT
import           Mirza.Common.Types                     as CT
import           Mirza.Common.Utils                     (newUUID,
                                                         notImplemented)

import           Data.GS1.EPC                           (GS1CompanyPrefix (..))

import           Servant                                hiding (header)
import           Servant.Swagger.UI

import qualified Data.Pool                              as Pool
import           Database.PostgreSQL.Simple

import           Network.Wai                            (Middleware)
import qualified Network.Wai.Handler.Warp               as Warp

import           Data.ByteString                        (ByteString)
import           Data.Semigroup                         ((<>))
import           Data.Text                              (pack)
import           Options.Applicative

import qualified Crypto.Scrypt                          as Scrypt

import           Control.Exception                      (finally)
import           Katip                                  as K
import           System.IO                              (stdout)



defaultPortNumber :: Int
defaultPortNumber = 8000

defaultDatabaseConnectionString :: ByteString
defaultDatabaseConnectionString = "dbname=devMirzaBusinessRegistry"



data ServerOptions = ServerOptions
  { soGlobals  :: GlobalOptions
  , soExecMode :: ExecMode
  }

data ExecMode
  = RunServer RunServerOptions
  | InitDb
  | UserAction UserCommand
  | BusinessAction BusinessCommand

data GlobalOptions = GlobalOptions
  { goDbConnStr    :: ByteString
  , goScryptN      :: Integer
  , goScryptP      :: Integer
  , goScryptR      :: Integer
  , goLoggingLevel :: K.Severity
  , goEnvType      :: CT.EnvType
  }

data RunServerOptions = RunServerOptions
  { rsoPortNumber :: Int
  }

data UserCommand
  = AddUser

data BusinessCommand
  = BusinessAdd
  | BusinessList



main :: IO ()
main = multiplexGlobalOptions =<< execParser opts where
  opts = Options.Applicative.info (serverOptions <**> helper)
    (fullDesc
    <> progDesc "todo some description"
    <> header "Supply Chain Business Registry Service")


-- Handles the overriding global options (this effectively defines the point
-- where the single binary could be split into multiple binaries.
multiplexGlobalOptions :: ServerOptions -> IO ()
multiplexGlobalOptions (ServerOptions globals mode) = case mode of
  InitDb             -> runMigration globals
  RunServer opts     -> launchServer globals opts
  UserAction AddUser -> notImplemented
  BusinessAction bc  -> runBusinessCommand globals bc


launchServer :: GlobalOptions -> RunServerOptions -> IO ()
launchServer globals options = do
      let portNumber = rsoPortNumber options
      ctx <- initBRContext globals
      app <- initApplication globals options ctx
      mids <- initMiddleware globals options
      putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/"
      Warp.run (fromIntegral portNumber) (mids app) `finally` closeScribes (BT._brKatipLogEnv ctx)


initBRContext :: GlobalOptions -> IO BT.BRContext
initBRContext (GlobalOptions dbConnStr n p r lev envT) = do
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


initApplication :: GlobalOptions -> RunServerOptions -> BT.BRContext -> IO Application
initApplication _go _so ev =
  pure $ serveWithContext api
          (basicAuthServerContext ev)
          (server ev)


initMiddleware :: GlobalOptions -> RunServerOptions -> IO Middleware
initMiddleware _ _ = pure id


runBusinessCommand :: GlobalOptions -> BusinessCommand -> IO ()
runBusinessCommand globals BusinessList = do
  ctx <- initBRContext globals
  ebizs <- runAppM ctx $ runDb listBusinessesQuery
  either (print @BusinessRegistryError) (mapM_ print) ebizs

runBusinessCommand globals BusinessAdd = do
  business_id <- newUUID
  biz_gs1_company_prefix <- GS1CompanyPrefix . pack <$>  prompt "GS1CompanyPrefix"
  biz_name      <- pack <$> prompt "Name     "
  biz_function  <- pack <$> prompt "Function "
  biz_site_name <- pack <$> prompt "Site name"
  biz_address   <- pack <$> prompt "Address  "
  biz_lat       <- read <$> prompt "Lat      "
  biz_long      <- read <$> prompt "Long     "
  let newBiz = BusinessT{..}
  ctx <- initBRContext globals
  ebiz <- runAppM ctx $ runDb (addBusinessQuery newBiz)
  either (print @BusinessRegistryError) print ebiz

runMigration :: GlobalOptions -> IO ()
runMigration opts = do
  ctx <- initBRContext opts
  res <- Schema.runMigrationInteractive @BRContext @SqlError ctx
  print res

prompt :: String -> IO String
prompt str = putStrLn str *> getLine





-- Implementation
server :: BRContext -> Server API
server ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[BasicAuthCheck BT.AuthUser])
        (appMToHandler ev)
        (appHandlers @BRContext @BusinessRegistryError)



-- This is a debug function for activating development test stub functions.
-- TODO: Remove this stub before release.
debugFunc :: IO()
debugFunc = do
  putStrLn ("Running Debug Option")
  -- Debug test code goes here...



--------------------------------------------------------------------------------
-- Command Line Options Argument Parsers
--------------------------------------------------------------------------------

standardCommand :: String -> Parser a -> String -> Mod CommandFields a
standardCommand name action desciption =
  command name (info (action <**> helper) (progDesc desciption))


serverOptions :: Parser ServerOptions
serverOptions = ServerOptions
  <$> globalOptions
  <*> subparser
        ( mconcat
          [ standardCommand "initdb"   initDb "Initialise the Database"
          , standardCommand "adduser"  addUser "Interactively add new users"
          , standardCommand "server"   runServer "Run HTTP server"
          , standardCommand "business" businessCommand "Operations on businesses"
          ]
        )


runServer :: Parser ExecMode
runServer = RunServer <$>
  (RunServerOptions
  <$> option auto
      (
          long "port"
      <>  help "Port to run the service on."
      <>  showDefault
      <>  value defaultPortNumber
      )
  )


globalOptions :: Parser GlobalOptions
globalOptions = GlobalOptions
  <$> strOption
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
    <*> option auto
      ( long "env" <> short 'e'
      <> value Dev <> showDefault
      <> help "Environment, Dev | Prod"
      )


-- TODO: Add flag to confirm change to database
initDb :: Parser ExecMode
initDb = pure InitDb


addUser :: Parser ExecMode
addUser = pure $ UserAction AddUser


businessCommand :: Parser ExecMode
businessCommand = BusinessAction <$> businessCommands


businessCommands :: Parser BusinessCommand
businessCommands = subparser
  ( mconcat
    [ standardCommand "add"  addBusiness  "Add a new business to the registry"
    , standardCommand "list" listBusiness "List all businesses and their Ids"
    ]
  )


addBusiness :: Parser BusinessCommand
addBusiness = pure BusinessAdd


listBusiness :: Parser BusinessCommand
listBusiness = pure BusinessList
