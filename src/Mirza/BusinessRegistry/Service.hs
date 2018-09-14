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
  , module Handlers
 ) where

import           Mirza.BusinessRegistry.API

import           Mirza.BusinessRegistry.Handlers.Business as Handlers
import           Mirza.BusinessRegistry.Handlers.Common   as Handlers
import           Mirza.BusinessRegistry.Handlers.Keys     as Handlers
import           Mirza.BusinessRegistry.Handlers.Users    as Handlers
import           Mirza.BusinessRegistry.Types
import           Mirza.Common.Utils

import           Servant
import           Servant.Swagger

import           GHC.TypeLits                             (KnownSymbol)

import           Control.Lens                             hiding ((.=))
import           Control.Monad.IO.Class                   (liftIO)

import           Data.ByteString.Lazy.Char8               as BSL8
import qualified Data.HashMap.Strict.InsOrd               as IOrd
import           Text.Printf                              (printf)

import           Data.Swagger


-- All possible error types that could be thrown through the handlers.
type PossibleErrors err = (AsKeyError err)


appHandlers :: (BRApp context err, HasScryptParams context, PossibleErrors err)
            => ServerT ServerAPI (AppM context err)
appHandlers = publicServer :<|> privateServer

publicServer :: (BRApp context err, HasScryptParams context, PossibleErrors err)
             => ServerT PublicAPI (AppM context err)
publicServer =
       getPublicKey
  :<|> getPublicKeyInfo
  :<|> listBusinesses


privateServer :: (BRApp context err, HasScryptParams context,  PossibleErrors err)
              => ServerT ProtectedAPI (AppM context err)
privateServer =
       addUserAuth
  :<|> addBusinessAuth
  :<|> addPublicKey
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


appMToHandler :: forall x context. context -> AppM context BusinessRegistryError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ runAppM context act
  case res of
    Left err -> appErrToHttpErr err
    Right a  -> return a


-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Business Registry Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")


-- | Takes in a BusinessRegistryError and converts it to an HTTP error (eg. err400)
appErrToHttpErr :: BusinessRegistryError -> Handler a
appErrToHttpErr (KeyErrorBRE kError) = keyErrToHttpErr kError
appErrToHttpErr x@(DBErrorBRE _sqlError)              = liftIO (print x) >> notImplemented
appErrToHttpErr x@(GS1CompanyPrefixExistsBRE)         = liftIO (print x) >> notImplemented
appErrToHttpErr x@(BusinessDoesNotExistBRE)           = liftIO (print x) >> notImplemented
appErrToHttpErr x@(UserCreationErrorBRE _reason)      = liftIO (print x) >> notImplemented
appErrToHttpErr x@(UnexpectedErrorBRE _reason)        = liftIO (print x) >> notImplemented

keyErrToHttpErr :: KeyError -> Handler a
keyErrToHttpErr (InvalidRSAKey _) =
  throwError $ err400 {
    errBody = "Failed to parse RSA Public key."
  }
keyErrToHttpErr (InvalidRSAKeySize (Expected (Bit expSize)) (Received (Bit recSize))) =
  throwError $ err400 {
    errBody = BSL8.pack $ printf "Invalid RSA Key size. Expected: %d Bits, Received: %d Bits\n" expSize recSize
  }
keyErrToHttpErr KeyAlreadyRevoked =
  throwError $ err400 { errBody = "Public Key already revoked" }
keyErrToHttpErr KeyAlreadyExpired =
  throwError $ err400 { errBody = "Public Key already expired" }
keyErrToHttpErr UnauthorisedKeyAccess =
  throwError $ err403 { errBody = "Not authorised to access this key." }
keyErrToHttpErr (PublicKeyInsertionError _) =
  throwError $ err500 { errBody = "Key could not be inserted." }
keyErrToHttpErr (KeyNotFound _) =
  throwError $ err404 { errBody = "Key with the given ID not found." }
