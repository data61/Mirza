{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Lib
import Migrate
import Data.ByteString

import Options.Applicative
import Data.Semigroup ((<>))

data ServerOptions = ServerOptions
  { debug      :: Bool
  , initDB     :: Bool
  , connectionStr :: ByteString
  , port :: Int
  , uiFlavour :: UIFlavour
  }

serverOptions :: Parser ServerOptions
serverOptions = ServerOptions
      <$> switch
          ( long "debug"
         <> help "Print Databse Debug Messages" )
      <*> switch
          ( long "initialise database"
         <> short 'i'
         <> help "Put empty tables into a fresh database" )
      <*> option auto
          ( long "connectionString"
         <> help "database connection string"
         <> showDefault
         <> value "dbname=testsupplychainserver")
       <*> option auto
          ( long "port"
         <> help "Port to run database on"
         <> showDefault
         <> value 8080)
       <*> option auto
          ( long "uiFlavour"
         <> help "Use jensoleg or Original UI Flavour for the Swagger API"
         <> showDefault
         <> value JensOleG)




main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (serverOptions <**> helper)
      (fullDesc
      <> progDesc "Run a supply chain server"
      <> header "SupplyChainServer - A server for capturing GS1 events and recording them on a blockchain")


runProgram :: ServerOptions -> IO ()
runProgram (ServerOptions debug False connectionStr port flavour) = startApp connectionStr debug port flavour
runProgram _ = migrate

-- main = migrate -- make this a command line argument
-- cmd args -->
    -- run_schema
    -- env (prod/dev --> create DBFunc based on this)
    -- connectionStr
