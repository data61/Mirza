{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}


module Mirza.Trails.Main where


import           Mirza.Trails.API
import           Mirza.Trails.Service
import           Mirza.Trails.Types

import           Mirza.Common.Types

import           Servant
import           Servant.Swagger.UI

import qualified Data.Pool                   as Pool
import           Database.PostgreSQL.Simple

import           Network.Wai                 (Middleware)
import qualified Network.Wai.Handler.Warp    as Warp
import qualified Network.Wai.Middleware.Cors as CorsMiddleware

import           Data.ByteString             (ByteString)
import           Data.Semigroup              ((<>))
import           Data.Text                   (pack)
import           Options.Applicative         hiding (action)

import           Control.Exception           (finally)
import           Data.Maybe                  (fromMaybe)
import           Katip                       as K
import           System.IO                   (FilePath, IOMode (AppendMode),
                                              hPutStr, openFile, stderr, stdout)


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Port number changed so that OR and SCS can be run at the same time
defaultPortNumber :: Int
defaultPortNumber = 8300

defaultDatabaseConnectionString :: ByteString
defaultDatabaseConnectionString = "dbname=devtrails"

corsOrigins :: [CorsMiddleware.Origin]
corsOrigins = [
    "http://localhost:8080"
  , "https://demo.mirza.d61.io"
  ]

--------------------------------------------------------------------------------
-- Command Line Options Data Types
--------------------------------------------------------------------------------
data InitOptionsTrails = InitOptionsTrails
  { iotGlobals  :: ServerOptionsTrails
  , iotExecMode :: ExecMode
  }

data ExecMode
  = RunServer RunServerOptions
  | InitDb

data ServerOptionsTrails = ServerOptionsTrails
  { mandatoryOptionsDbConnStr    :: ByteString
  , mandatoryOptionsLoggingLevel :: K.Severity
  , mandatoryOptionsLogLocation  :: Maybe FilePath
  , mandatoryOptionsEnvType      :: EnvType
  }

data RunServerOptions = RunServerOptions
  { runServerOptionsPortNumber    :: Int
  }


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = multiplexInitOptions =<< execParser opts where
  opts = Options.Applicative.info (serverOptions <**> helper)
    (fullDesc
    <> progDesc "Here to meet all your trail needs"
    <> header "Trails Service")


-- Handles the overriding server options (this effectively defines the point
-- where the single binary could be split into multiple binaries.
multiplexInitOptions :: InitOptionsTrails -> IO ()
multiplexInitOptions (InitOptionsTrails opts mode) = case mode of
  RunServer rsOpts -> launchServer opts rsOpts
  InitDb           -> runMigration opts


--------------------------------------------------------------------------------
-- Service
--------------------------------------------------------------------------------

launchServer :: ServerOptionsTrails -> RunServerOptions -> IO ()
launchServer opts rso = do
      let portNumber = runServerOptionsPortNumber rso
      context <- initTrailsContext opts
      app <- initApplication context
      mids <- initMiddleware opts rso
      putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/"
      Warp.run (fromIntegral portNumber) (mids app) `finally` closeScribes (_trailsKatipLogEnv context)

initTrailsContext :: ServerOptionsTrails -> IO TrailsContext
initTrailsContext (ServerOptionsTrails dbConnStr lev mlogPath envT) = do
  logHandle <- maybe (pure stdout) (flip openFile AppendMode) mlogPath
  hPutStr stderr $ "(Logging will be to: " ++ fromMaybe "stdout" mlogPath ++ ") "
  handleScribe <- mkHandleScribe ColorIfTerminal logHandle lev V3
  logEnv <- initLogEnv "trailsService" (Environment . pack . show $ envT)
            >>= registerScribe "stdout" handleScribe defaultScribeSettings
  connpool <- Pool.createPool (connectPostgreSQL dbConnStr) close
                      1 -- Number of "sub-pools",
                      60 -- How long in seconds to keep a connection open for reuse
                      10 -- Max number of connections to have open at any one time
                      -- TODO: Make this a config paramete
  pure $ TrailsContext envT connpool logEnv mempty mempty


initApplication :: TrailsContext -> IO Application
initApplication ev =
  pure $ serve api (server ev)


myCors :: Middleware
myCors = CorsMiddleware.cors (const $ Just policy)
    where
      policy = CorsMiddleware.simpleCorsResourcePolicy
        { CorsMiddleware.corsRequestHeaders = ["Content-Type", "Authorization"]
        , CorsMiddleware.corsMethods = "PUT" : CorsMiddleware.simpleMethods
        , CorsMiddleware.corsOrigins = Just (corsOrigins, True)
        }

initMiddleware :: ServerOptionsTrails -> RunServerOptions -> IO Middleware
initMiddleware _ _ = pure myCors

-- Implementation
server :: TrailsContext -> Server API
server context =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServer
        (Proxy @ServerAPI)
        (appMToHandler context)
        (appHandlers)


--------------------------------------------------------------------------------
-- Migration Command
--------------------------------------------------------------------------------

runMigration :: ServerOptionsTrails -> IO ()
runMigration opts = do
  _ctx <- initTrailsContext opts
  --res <- runMigrationWithConfirmation @ORContextMinimal @SqlError ctx interactiveMigrationConfirm
  --print res
  pure ()

--------------------------------------------------------------------------------
-- Command Line Options Argument Parsers
--------------------------------------------------------------------------------

standardCommand :: String -> Parser a -> String -> Mod CommandFields a
standardCommand name action desciption =
  command name (info (action <**> helper) (progDesc desciption))


-- The standard format of the main command line options is [Command] [Action], this applies to things like org and user.
serverOptions :: Parser InitOptionsTrails
serverOptions = InitOptionsTrails
  <$> parsedServerOptions
  <*> subparser
        ( mconcat
          [ standardCommand "server"    runServer "Run HTTP server"
          , standardCommand "initdb"    initDb "Initialise the Database (Note: This command only works if the database \
                                               \is empty and can't be used for migrations or if the database already \
                                               \contains the schema."
          ]
        )


runServer :: Parser ExecMode
runServer = RunServer <$> (RunServerOptions
  <$> option auto
    (
       long "port"
    <> help "Port to run the service on."
    <> showDefault
    <> value defaultPortNumber
    )
  )

parsedServerOptions :: Parser ServerOptionsTrails
parsedServerOptions = ServerOptionsTrails
  <$> strOption
    (
         long "conn"
    <>  short 'c'
    <>  help "Database connection string in libpq format. See: https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
    <>  showDefault
    <>  value defaultDatabaseConnectionString
    )
  <*> option auto
    (  long "log-level"
    <> value InfoS
    <> showDefault
    <> help ("Logging level: " ++ show [minBound .. maxBound :: Severity])
    )
  <*> optional (strOption
    (  long "log-path"
    <> short 'l'
    <> help "Path to write log output to (defaults to stdout)"
    ) )
  <*> option auto
    ( long "env" <> short 'e'
    <> value Dev <> showDefault
    <> help "Environment, Dev | Prod"
    )


-- TODO: Add flag to change from interactive confirmation to instead be automatic operation (so this command can be used from scripts or whatnot) (e.g. runIfSafe) .
initDb :: Parser ExecMode
initDb = pure InitDb
