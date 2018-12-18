{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Main where

import           Mirza.SupplyChain.API
import           Mirza.SupplyChain.Auth
import           Mirza.SupplyChain.Database.Migrate
import           Mirza.SupplyChain.Service
import           Mirza.SupplyChain.Types            (AppError, EnvType (..),
                                                     SCSContext (..), User)

import qualified Mirza.SupplyChain.Types            as ST

import           Servant                            hiding (header)
import           Servant.Client
import           Servant.Swagger.UI

import qualified Data.Pool                          as Pool
import           Database.PostgreSQL.Simple

import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp

import           Data.ByteString                    (ByteString)
import           Data.Text                          (pack)

import           Data.Semigroup                     ((<>))
import           Options.Applicative

import           Control.Lens

import qualified Crypto.Scrypt                      as Scrypt

import           Control.Exception                  (finally)
import           Data.Maybe                         (fromMaybe)
import           Katip                              as K

import           System.Exit                        (exitFailure)
import           System.IO                          (IOMode (AppendMode),
                                                     hPutStrLn, openFile,
                                                     stderr, stdout)

data ServerOptionsSCS = ServerOptionsSCS
  { env           :: EnvType
  , initDB        :: Bool
  , connectionStr :: ByteString
  , scsPort       :: Int
  , sScryptN      :: Integer
  , sScryptP      :: Integer
  , sScryptR      :: Integer
  , loggingLevel  :: K.Severity
  , brServiceInfo :: Maybe (String, Int) -- Maybe (brhost, brport)
  , loggingPath   :: Maybe FilePath
  }

defaultDbConnectionStr :: ByteString
defaultDbConnectionStr = "dbname=devsupplychainserver"

serverOptions :: Parser ServerOptionsSCS
serverOptions = ServerOptionsSCS
      <$> option auto
          ( long "env" <> short 'e'
          <> value Dev <> showDefault
          <> help "Environment, Dev | Prod"
          )
      <*> switch
          ( long "init-db"
         <> help "Put empty tables into a fresh database" )
      <*> strOption
          ( long "conn" <> short 'c' <> showDefault
          <> help "Database connection string in libpq format. See: https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
          <> value defaultDbConnectionStr)
      <*> option auto
          ( long "scsport" <> short 'p' <> showDefault <> value 8000
          <> help "Port to run database on" )
      <*> option auto
          (long "scryptN" <> value 14 <> showDefault
          <> help "Scrypt N parameter (>= 14)")
      <*> option auto
          (long "scryptP" <> value 8 <> showDefault
          <> help "Scrypt r parameter (>= 8)")
      <*> option auto
          (long "scryptR" <> value 1 <> showDefault
          <> help "Scrypt r parameter (>= 1)")
      <*> option auto
          (long "log-level" <> value InfoS <> showDefault
          <> help ("Logging level: " ++ show [minBound .. maxBound :: Severity]))
      <*> optional ((,) <$>
            strOption (
            (long "brhost" <> help "The host to run the business registry on"))
            <*>
            option auto (long "brport" <> help "Port to run business registry on")
          )
      <*> optional (strOption
          (  long "log-path"
          <> short 'l'
          <> help "Path to write log output to (defaults to stdout)"
          )
        )


main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (serverOptions <**> helper)
      (fullDesc
      <> progDesc "Run a supply chain server"
      <> header "SupplyChainServer - A server for capturing GS1 events and recording them on a blockchain")

runProgram :: ServerOptionsSCS -> IO ()
runProgram so@ServerOptionsSCS{initDB = False, scsPort, brServiceInfo =Just __} = do
  ctx <- initSCSContext so
  app <- initApplication so ctx
  mids <- initMiddleware so
  putStrLn $ "http://localhost:" <> show scsPort <> "/swagger-ui/"
  Warp.run (fromIntegral scsPort) (mids app) `finally` closeScribes (ctx ^. ST.scsKatipLogEnv)
runProgram ServerOptionsSCS{initDB = False, brServiceInfo = Nothing} = do
  hPutStrLn stderr $ "Required unless initialising the database: --brhost ARG --brport ARG"
  exitFailure
runProgram so = migrate $ connectionStr so

initMiddleware :: ServerOptionsSCS -> IO Middleware
initMiddleware _ = pure id

initSCSContext :: ServerOptionsSCS -> IO ST.SCSContext
initSCSContext (ServerOptionsSCS envT _ dbConnStr _prt n p r lev (Just (brHost, bizRegPort)) mlogPath) = do
  logHandle <- maybe (pure stdout) (flip openFile AppendMode) mlogPath
  hPutStrLn stderr $ "Logging will be to: " <> fromMaybe "stdout" mlogPath
  handleScribe <- mkHandleScribe ColorIfTerminal logHandle lev V3
  logEnv <- initLogEnv "supplyChainServer" (Environment . pack . show $ envT)
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
                      -- TODO: Make this a config parameter
  manager <- newManager defaultManagerSettings
  let scheme = if envT == Prod then Https else Http
      baseUrl = BaseUrl scheme brHost bizRegPort ""
  pure $ SCSContext
          envT
          connpool
          params
          logEnv
          mempty
          mempty
          (mkClientEnv manager baseUrl)
initSCSContext _ = do -- this should never happen
  hPutStrLn stderr $ "Required unless initialising the database: --brhost ARG --brport ARG"
  exitFailure

initApplication :: ServerOptionsSCS -> ST.SCSContext -> IO Application
initApplication _so ev =
  pure $ serveWithContext api
          (basicAuthServerContext ev)
          (server' ev)

server' :: SCSContext -> Server API
server' ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[BasicAuthCheck User])
        (appMToHandler ev)
        (appHandlers @SCSContext @AppError)
