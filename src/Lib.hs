{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Lib
    ( startApp,
      startApp_nomain,
      UIFlavour(..)
    )
    where


import Prelude        ()
import Prelude.Compat
import qualified AppConfig as AC

-- import Control.Monad.IO.Class
-- import Control.Monad.Logger (runStderrLoggingT)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT,
                                         asks, ask, liftIO)
import Servant
import Servant.Server.Experimental.Auth()
-- import Servant.Swagger
import Servant.Swagger.UI

-- import GHC.TypeLits (KnownSymbol)

-- import Data.Aeson
-- import Data.Aeson.TH
import Data.Swagger
import Data.Maybe
-- import Data.GS1.Event
-- import Data.GS1.EPC
-- import Data.GS1.DWhen
-- import Data.GS1.DWhere
-- import Data.GS1.DWhat
-- import Data.GS1.DWhy
-- import Data.Either.Combinators
-- import Data.Time
-- import Data.String.Conversions
import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple.Connection
-- import Data.ByteString hiding (elem)
import Data.ByteString (ByteString)
-- import qualified Data.ByteString (ByteString) as ByteString
-- import qualified Data.HashMap.Strict.InsOrd as IOrd
import qualified Network.Wai.Handler.Warp as Warp
-- import Network.Wai

import Control.Lens       hiding ((.=))

-- import GHC.Generics       (Generic)

import System.Environment (getArgs, lookupEnv)

import Text.Read          (readMaybe)

import           API
import           Service
import qualified AppConfig as AC
import           GHC.Word        (Word16)


startApp :: ByteString -> Bool -> Word16 -> UIFlavour-> IO ()
startApp dbConnStr isDebug port uiFlavour = do
    conn <- connectPostgreSQL dbConnStr
    let envT = AC.mkEnvType isDebug
        env  = AC.Env envT conn
        app = return $ webApp env uiFlavour
    putStrLn $ "http://localhost:" ++ show port ++ "/" ++ "swagger-ui/"
    Warp.run (fromIntegral port) =<< app

-- easily start the app in ghci, no command line arguments required.
startApp_nomain :: ByteString -> IO ()
startApp_nomain dbConnStr = startApp dbConnStr True 8000 Original

{-
app :: UIFlavour -> Application
app = (serveWithContext api basicAuthServerContext) . server'
-}


-- Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
webApp :: AC.Env -> UIFlavour -> Application
webApp env uiFlavour  = serveWithContext api basicAuthServerContext (server' env uiFlavour)

-- | ``nt`` stands for natural transformation
-- here, we are transforming from AppM to Handler
appMToHandler :: forall x. AC.Env -> AC.AppM x -> Handler x
appMToHandler env = flip runReaderT env . AC.unAppM


{-
mkApp :: FilePath -> UIFlavour ->  IO Application
mkApp dbConnStr uiFlavour = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs dbConnStr) 5
  runSqlPool (runMigration migrate) pool
  return $ app pool uiFlavour
  -}

-- @todo - make this a command line argument
connectionStr :: ByteString
connectionStr = "dbname=testsupplychainserver"

mkApp :: ByteString -> UIFlavour ->  IO Application
mkApp dbConnStr uiFlavour = do
  conn <- connectPostgreSQL dbConnStr
--   createTables conn
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
    deriving (Eq, Read, Show)

server' :: Connection -> UIFlavour -> Server API'
server' conn uiFlavour = server Normal
    :<|> server Nested
    :<|> schemaUiServer (serveSwaggerAPI' SpecDown)
  where
    appProxy = Proxy :: Proxy AC.AppM
    server :: Variant -> Server API
    server variant =
      schemaUiServer (serveSwaggerAPI' variant) :<|> (privateServer conn :<|> publicServer)
    nt = appMToHandler
    mainServer = hoistServer appProxy nt (server Normal)
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

