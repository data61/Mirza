{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Lib
    ( startApp,
      startApp_nomain
    )
    where


import Prelude        ()
import Prelude.Compat


import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)

import Servant
import Servant.Server.Experimental.Auth()
import Servant.Swagger
import Servant.Swagger.UI

import GHC.TypeLits (KnownSymbol)

import Data.Aeson
import Data.Aeson.TH
import Data.Swagger
import Data.Maybe
import Data.GS1.Event
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy
import Data.Either.Combinators
import Data.Time
import Data.String.Conversions

import qualified Data.ByteString as ByteString
import qualified Data.HashMap.Strict.InsOrd as IOrd
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai

import Control.Lens       hiding ((.=))

import GHC.Generics       (Generic)

import System.Environment (getArgs, lookupEnv)

import Text.Read          (readMaybe)


import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes

import API
import Model
import Storage as S
import Service


startApp :: FilePath -> IO ()
startApp sqliteFile = do
    args <- getArgs
    let uiFlavour = if "jensoleg" `elem` args then JensOleG else Original
    case args of
        ("run":_) -> do
            p <- fromMaybe 8000 . (>>= readMaybe) <$> lookupEnv "PORT"
            putStrLn $ "http://localhost:" ++ show p ++ "/" ++ "swagger-ui/"
            Warp.run p =<< mkApp sqliteFile uiFlavour
        _ -> do
            putStrLn "Example application, used as a compilation check"
            putStrLn "To run, pass run argument: --test-arguments run"


-- easily start the app in ghci, no command line arguments required.
startApp_nomain :: FilePath -> IO ()
startApp_nomain filePath = Warp.run 8000 =<< (mkApp filePath Original)

{-
app :: UIFlavour -> Application
app = (serveWithContext api basicAuthServerContext) . server'
-}


webApp :: Sql.Connection -> UIFlavour -> Application
webApp conn = serveWithContext api (basicAuthServerContext conn) . server' conn


{-
mkApp :: FilePath -> UIFlavour ->  IO Application
mkApp sqliteFile uiFlavour = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5
  runSqlPool (runMigration migrate) pool
  return $ app pool uiFlavour
  -}


mkApp :: FilePath -> UIFlavour ->  IO Application
mkApp sqliteFile uiFlavour = do
  conn <- Sql.open sqliteFile
  createTables conn
  return (webApp conn uiFlavour)




-- Implementation

-- | We test different ways to nest API, so we have an enumeration
data Variant
    = Normal
    | Nested
    | SpecDown
    deriving (Eq)

data UIFlavour
    = Original
    | JensOleG
    deriving (Eq)

server' :: Sql.Connection -> UIFlavour -> Server API'
server' conn uiFlavour = server Normal
    :<|> server Nested
    :<|> schemaUiServer (serveSwaggerAPI' SpecDown)
  where
    server :: Variant -> Server API
    server variant =
      schemaUiServer (serveSwaggerAPI' variant) :<|> (privateServer conn :<|> publicServer conn)

    schemaUiServer
        :: (Server api ~ Handler Swagger)
        => Swagger -> Server (SwaggerSchemaUI' dir api)
    schemaUiServer = case uiFlavour of
        Original -> swaggerSchemaUIServer
        JensOleG -> jensolegSwaggerSchemaUIServer

    serveSwaggerAPI' Normal    = serveSwaggerAPI
    serveSwaggerAPI' Nested    = serveSwaggerAPI
        & basePath ?~ "/nested"
        & info.description ?~ "Nested API"
    serveSwaggerAPI' SpecDown  = serveSwaggerAPI
        & info.description ?~ "Spec nested"

