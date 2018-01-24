{-# LANGUAGE OverloadedStrings     #-}
module Main where

import Lib
import Migrate
import Data.ByteString

import Options.Applicative
import Data.Semigroup ((<>))

data ServerOptions = ServerOptions
  { verbose       :: Bool
  , initDB        :: Bool
  , connectionStr :: ByteString
  , port          :: Int
  , uiFlavour     :: UIFlavour
  }

serverOptions :: Parser ServerOptions
serverOptions = ServerOptions
      <$> switch
          ( long "verbose"
         <> short 'v'
         <> help "Print Databse Debug Messages" )
      <*> switch
          ( long "init-db"
         <> short 'i'
         <> help "Put empty tables into a fresh database" )
      <*> option auto
          ( long "conn"
         <> short 'c'
         <> help "database connection string"
         <> showDefault
         <> value defConnectionStr)
       <*> option auto
          ( long "port"
         <> help "Port to run database on"
         <> showDefault
         <> value 8000)
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
runProgram (ServerOptions isDebug False connStr portNum flavour) =
    startApp connStr isDebug (fromIntegral portNum) flavour
runProgram _ = migrate defConnectionStr

defConnectionStr :: ByteString
defConnectionStr = "dbname=testsupplychainserver"
