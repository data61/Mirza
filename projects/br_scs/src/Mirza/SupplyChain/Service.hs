{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- | Endpoint definitions go here. Most of the endpoint definitions are
-- light wrappers around functions in BeamQueries
module Mirza.SupplyChain.Service
  ( appHandlers
  , appMToHandler
  , serveSwaggerAPI
  ) where

import           Mirza.Common.Utils

import           Mirza.SupplyChain.API
import           Mirza.SupplyChain.ErrorUtils                 (appErrToHttpErr)

import           Mirza.SupplyChain.Handlers.EventRegistration as Handlers
import           Mirza.SupplyChain.Handlers.Health            as Handlers
import           Mirza.SupplyChain.Handlers.Queries           as Handlers
import           Mirza.SupplyChain.Handlers.Signatures        as Handlers
import           Mirza.SupplyChain.Handlers.UXUtils           as Handlers

import           Mirza.BusinessRegistry.Types                 (AsBRError)
import           Mirza.SupplyChain.Types

import           Servant
import           Servant.Swagger

import           GHC.TypeLits                                 (KnownSymbol)

import           Control.Lens
import           Control.Monad.IO.Class                       (liftIO)
import qualified Data.HashMap.Strict.InsOrd                   as IOrd
import           Data.Swagger

import           Mirza.Common.GS1BeamOrphans                  ()

import qualified Crypto.JOSE                                  as JOSE


appHandlers :: (Member context '[HasDB, HasBRClientEnv],
                Member err     '[JOSE.AsError, AsServiceError, AsServantError, AsBRError, AsSqlError])
            => ServerT ServerAPI (AppM context err)
appHandlers =
  -- Health
       health
  -- Users
  :<|> versionInfo

-- Signatures
  :<|> eventSign
-- Queries
  :<|> listEvents
  :<|> eventInfo
-- Event Registration
  :<|> insertObjectEvent
  :<|> insertAggEvent
  :<|> insertTransactEvent
  :<|> insertTransfEvent
  :<|> listEventsPretty

instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ =
    let
      authSchemes = IOrd.singleton "basic" $ SecurityScheme SecuritySchemeBasic Nothing
      securityRequirements = [SecurityRequirement $ IOrd.singleton "basic" []]
    in
      toSwagger (Proxy :: Proxy sub)
      & securityDefinitions .~ authSchemes
      & allOperations . security .~ securityRequirements



appMToHandler :: forall x context. context -> AppM context AppError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ runAppM context act
  case res of
    Left (AppError e) -> appErrToHttpErr e
    Right a           -> pure a

-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Supplychain Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")

