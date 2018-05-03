module Main where

import           Mirza.SupplyChain.AppConfig (EnvType (..))
import           Mirza.SupplyChain.Lib
import           Mirza.SupplyChain.Migrate   (defConnectionStr, migrate)

import           Data.ByteString             (ByteString)
import           Data.Semigroup              ((<>))
import           Options.Applicative

import qualified Crypto.Scrypt               as Scrypt

data ServerOptions = ServerOptions
  { env           :: EnvType
  , initDB        :: Bool
--  , clearDB       :: Bool
  , connectionStr :: ByteString
  , port          :: Int
  , uiFlavour     :: UIFlavour
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
          ( long "uiFlavour" <> showDefault
         <> help "Use jensoleg or Original UI Flavour for the Swagger API"
         <> value Original)
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
runProgram (ServerOptions envT False connStr portNum flavour n p r) =
  case Scrypt.scryptParams (max n 14) (max p 8) (max r 1) of
    Nothing -> do
      putStrLn $ unwords
        ["Invalid Scrypt params: ", show (n,p,r)
        ,"\nUsing default parameters"
        ]
      startApp connStr envT (fromIntegral portNum) flavour Scrypt.defaultParams
    Just params ->
      startApp connStr envT (fromIntegral portNum) flavour params
runProgram _ = migrate defConnectionStr
