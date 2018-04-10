{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}


module Lib
    ( startApp,
      startApp_nomain,
      UIFlavour(..)
    )
    where

import qualified AppConfig                        as AC
import           Prelude                          ()
import           Prelude.Compat

import           Servant
import           Servant.Server.Experimental.Auth ()
import           Servant.Swagger.UI

import           Control.Lens                     hiding ((.=))
import           Data.ByteString                  (ByteString)
import           Data.Swagger
import           Database.PostgreSQL.Simple
import qualified Network.Wai.Handler.Warp         as Warp

import           API
import           GHC.Word                         (Word16)
import           Service

startApp :: ByteString -> AC.EnvType -> Word16 -> UIFlavour -> IO ()
startApp dbConnStr envT prt uiFlavour = do
    conn <- connectPostgreSQL dbConnStr
    let
        env  = AC.Env envT conn
        app = return $ webApp env uiFlavour
    putStrLn $ "http://localhost:" ++ show prt ++ "/swagger-ui/"
    Warp.run (fromIntegral prt) =<< app

-- easily start the app in ghci, no command line arguments required.
startApp_nomain :: ByteString -> IO ()
startApp_nomain dbConnStr = startApp dbConnStr AC.Dev 8000 Original

-- Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
webApp :: AC.Env -> UIFlavour -> Application
webApp env uiFlavour  = serveWithContext api basicAuthServerContext (server' env uiFlavour)

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

server' :: AC.Env -> UIFlavour -> Server API'
server' env uiFlavour = server Normal
        :<|> server Nested
        :<|> schemaUiServer (serveSwaggerAPI' SpecDown)
  where

    -- appProxy = Proxy :: Proxy AC.AppM
    server :: Variant -> Server API
    server variant =
      schemaUiServer (serveSwaggerAPI' variant)
        :<|> enter (NT (appMToHandler env)) appHandlers
    -- mainServer = enter (appMToHandler env) (server Normal)
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
