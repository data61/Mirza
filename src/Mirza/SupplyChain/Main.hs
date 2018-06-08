{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Main where

import           Mirza.SupplyChain.API
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

data ServerOptions = ServerOptions
  { env           :: EnvType
  , initDB        :: Bool
--  , clearDB       :: Bool
  , connectionStr :: ByteString
  , port          :: Int
  , sScryptN      :: Integer
  , sScryptP      :: Integer
  , sScryptR      :: Integer
  }

serverOptions :: Parser ServerOptions
serverOptions = ServerOptions
      <$> option auto
          ( long "env" <> short 'e'
          <> value Dev <> showDefault
          <> help "Environment, Dev | Prod"
          )
      <*> switch
          ( long "init-db" <> short 'i'
         <> help "Put empty tables into a fresh database" )
    --   <*> switch
    --       ( long "clear-db"
    --      <> short 'e'
    --      <> help "Erase the database - DROP ALL TABLES" )
      <*> option auto
          ( long "conn" <> short 'c' <> showDefault
          <> help "database connection string"
          <> value defConnectionStr)
       <*> option auto
          ( long "port" <> showDefault <> value 8000
          <> help "Port to run database on"
          )
       <*> option auto
            (long "scryptN" <> value 14 <> showDefault
            <> help "Scrypt N parameter (>= 14)")
       <*> option auto
            (long "scryptP" <> value 8 <> showDefault
            <> help "Scrypt r parameter (>= 8)")
       <*> option auto
            (long "scryptR" <> value 1 <> showDefault
            <> help "Scrypt r parameter (>= 1)")


main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (serverOptions <**> helper)
      (fullDesc
      <> progDesc "Run a supply chain server"
      <> header "SupplyChainServer - A server for capturing GS1 events and recording them on a blockchain")


-- Sara's
-- runProgram :: ServerOptions -> IO ()
-- runProgram (ServerOptions isDebug False _connStr portNum flavour) =
--     startApp connStr isDebug (fromIntegral portNum) flavour
-- runProgram (ServerOptions _ _ True connStr portNum flavour) =
--     startApp connStr isDebug (fromIntegral portNum) flavour
-- runProgram _ = migrate defConnectionStr
runProgram :: ServerOptions -> IO ()
runProgram so@ServerOptions{initDB = False, port} = do
  app <- initApplication so
  mids <- initMiddleware so
  putStrLn $ "http://localhost:" ++ show port ++ "/swagger-ui/"
  Warp.run (fromIntegral port) $ mids app
-- FIXME: This is definitely wrong
runProgram _ = migrate defConnectionStr

initMiddleware :: ServerOptions -> IO Middleware
initMiddleware _ = pure id

-- initApplication :: ByteString -> AC.SCSContextType -> ScryptParams -> IO Application
initApplication :: ServerOptions -> IO Application
initApplication (ServerOptions envT _ dbConnStr _ n p r)  = do
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
  let ev  = SCSContext envT connpool params
      app = serveWithContext api
            (basicAuthServerContext ev)
            (server' ev)
  pure app

-- easily start the app in ghci, no command line arguments required.
startApp_nomain :: ByteString -> IO ()
startApp_nomain dbConnStr =
  initApplication (ServerOptions Dev False dbConnStr 8000 14 8 1) >>= Warp.run 8000


-- Implementation

server' :: SCSContext -> Server API
server' ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[BasicAuthCheck User])
        (appMToHandler ev)
        (appHandlers @SCSContext @AppError)
