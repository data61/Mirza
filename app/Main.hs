{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           Mirza.SupplyChain.API
import           Mirza.SupplyChain.Migrate  (defConnectionStr, migrate)
import           Mirza.SupplyChain.Model    (User)
import           Mirza.SupplyChain.Service
import           Mirza.SupplyChain.Types    (EnvType (..))
import qualified Mirza.SupplyChain.Types    as AC

import           Servant                    hiding (header)
import           Servant.Swagger.UI

import qualified Data.Pool                  as Pool
import           Database.PostgreSQL.Simple

import qualified Network.Wai.Handler.Warp   as Warp

import           Crypto.Scrypt              (ScryptParams, defaultParams)

import           Data.ByteString            (ByteString)
import           Data.Semigroup             ((<>))
import           GHC.Word                   (Word16)
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
runProgram (ServerOptions envT False connStr portNum n p r) =
  case Scrypt.scryptParams (max n 14) (max p 8) (max r 1) of
    Nothing -> do
      putStrLn $ unwords
        ["Invalid Scrypt params: ", show (n,p,r)
        ,"\nUsing default parameters"
        ]
      startApp connStr envT (fromIntegral portNum) Scrypt.defaultParams
    Just params ->
      startApp connStr envT (fromIntegral portNum) params
runProgram _ = migrate defConnectionStr







startApp :: ByteString -> AC.EnvType -> Word16 -> ScryptParams -> IO ()
startApp dbConnStr envT prt params = do
    connpool <- Pool.createPool (connectPostgreSQL dbConnStr) close
                        1 -- Number of "sub-pools",
                        60 -- How long in seconds to keep a connection open for reuse
                        10 -- Max number of connections to have open at any one time
                        -- TODO: Make this a config parameter

    let
        ev  = AC.Env envT connpool params
        app = webApp ev
    putStrLn $ "http://localhost:" ++ show prt ++ "/swagger-ui/"
    Warp.run (fromIntegral prt) app

-- easily start the app in ghci, no command line arguments required.
startApp_nomain :: ByteString -> IO ()
startApp_nomain dbConnStr = startApp dbConnStr AC.Dev 8000 defaultParams

-- Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
webApp :: AC.Env -> Application
webApp ev =
  serveWithContext api
    (basicAuthServerContext ev)
    (server' ev)

-- Implementation

server' :: AC.Env -> Server API
server' ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[BasicAuthCheck User])
        (appMToHandler ev)
        appHandlers
