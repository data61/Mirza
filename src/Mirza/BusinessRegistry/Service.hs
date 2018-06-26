{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Endpoint definitions go here. Most of the endpoint definitions are
-- light wrappers around functions in BeamQueries
module Mirza.BusinessRegistry.Service
 (
    appHandlers
  , publicServer
  , privateServer
  , appMToHandler
  , serveSwaggerAPI
 ) where

import           Mirza.BusinessRegistry.API
import           Mirza.SupplyChain.ErrorUtils             (appErrToHttpErr)

import           Mirza.BusinessRegistry.Handlers.Business as Handlers
import           Mirza.BusinessRegistry.Handlers.Common   as Handlers
import           Mirza.Common.Types
import qualified Mirza.SupplyChain.Types                  as STXXX

import           Servant
import           Servant.Swagger

import           GHC.TypeLits                             (KnownSymbol)

import           Control.Lens                             hiding ((.=))
import           Control.Monad.IO.Class                   (liftIO)
import qualified Data.HashMap.Strict.InsOrd               as IOrd
import           Data.Swagger



appHandlers :: (BRApp context err, HasScryptParams context) => ServerT ServerAPI (AppM context err)
appHandlers = publicServer :<|> privateServer

publicServer :: (BRApp context err, HasScryptParams context) => ServerT PublicAPI (AppM context err)
publicServer =
  -- Business
       getPublicKey
  :<|> getPublicKeyInfo
  :<|> listBusinesses

privateServer :: (BRApp context err) => ServerT ProtectedAPI (AppM context err)
privateServer =
-- Business
       addPublicKey
  :<|> revokePublicKey



instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ =
    let
      authSchemes = IOrd.singleton "basic" $ SecurityScheme SecuritySchemeBasic Nothing
      securityRequirements = [SecurityRequirement $ IOrd.singleton "basic" []]
    in
      toSwagger (Proxy :: Proxy sub)
      & securityDefinitions .~ authSchemes
      & allOperations . security .~ securityRequirements



appMToHandler :: forall x context. context -> AppM context STXXX.AppError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ runAppM context act
  case res of
    Left (STXXX.AppError e) -> appErrToHttpErr e
    Right a                 -> return a

-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Supplychain Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")
