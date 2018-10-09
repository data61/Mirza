{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

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
import           Mirza.BusinessRegistry.Handlers.Location as Handlers
import           Mirza.BusinessRegistry.Handlers.Users    as Handlers
import           Mirza.BusinessRegistry.Types
import           Mirza.Common.Utils

import           Katip

import           Servant
import           Servant.Swagger

import           GHC.TypeLits                             (KnownSymbol)

import           Control.Lens                             hiding ((.=))
import           Control.Monad.IO.Class                   (liftIO)
import           Control.Monad.Trans

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
  :<|> addLocation
  :<|> getLocationByGLN


instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ =
    let
      authSchemes = IOrd.singleton "basic" $ SecurityScheme SecuritySchemeBasic Nothing
      securityRequirements = [SecurityRequirement $ IOrd.singleton "basic" []]
    in
      toSwagger (Proxy :: Proxy sub)
      & securityDefinitions .~ authSchemes
      & allOperations . security .~ securityRequirements


appMToHandler :: (HasLogging context) => context -> AppM context BusinessRegistryError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ runAppM context act
  case res of
    Left err -> runKatipContextT (context ^. katipLogEnv) () (context ^. katipNamespace) (transformBRErrorAndLog err)
    Right a  -> return a


-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Business Registry Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")


-- | We log all errors for now so that developers have the oppertunity to skim the logs to look for potential issues.
-- The brError type contains all the information what we know about the error at this point so we add it in entirity
-- to the log.
transformBRErrorAndLog :: BusinessRegistryError -> KatipContextT Handler a
transformBRErrorAndLog brError = do
  $(logTM) InfoS (logStr $ show brError)
  lift (brErrorToHttpError brError)


throwHttpError :: ServantErr -> ByteString -> Handler a
throwHttpError httpStatus errorMessage = throwError $ httpStatus { errBody = errorMessage }

-- | Takes in a BusinessRegistryError and converts it to an HTTP error (eg. err400)
brErrorToHttpError :: BusinessRegistryError -> Handler a
brErrorToHttpError (KeyErrorBRE kError) = keyErrorToHttpError kError
brErrorToHttpError x@(DBErrorBRE _sqlError)       = unexpectedError
brErrorToHttpError x@(UnexpectedErrorBRE _reason) = unexpectedError
brErrorToHttpError x@(UnmatchedUniqueViolationBRE _sqlError) = unexpectedError
brErrorToHttpError x@(LocationNotKnownBRE) =
  throwHttpError err404 "Unknown GLN"
brErrorToHttpError (LocationExistsBRE) =
  throwHttpError err409 "Location already exists for this GLN"
brErrorToHttpError (GS1CompanyPrefixExistsBRE) =
  throwHttpError err400 "GS1 company prefix already exists."
brErrorToHttpError (BusinessDoesNotExistBRE) =
  throwHttpError err400 "Business does not exist."
brErrorToHttpError (UserCreationErrorBRE _ _) = userCreationError
brErrorToHttpError (UserCreationSQLErrorBRE _) = userCreationError

-- | A generic internal server error has occured. We include no more information in the result returned to the user to
-- limit further potential for exploitation, under the expectation that we log the errors to somewhere that is reviewed
-- regularly so that the development team are informed and can identify and patch the underlying issues.
unexpectedError :: Handler a
unexpectedError = throwHttpError err500 "An unknown error has occured."

-- | A common function for handling user errors uniformly irrespective of what the underlying cause is.
userCreationError :: Handler a
userCreationError = throwHttpError err400 "Unable to create user."


keyErrorToHttpError :: KeyError -> Handler a
keyErrorToHttpError (InvalidRSAKeyBRE _) =
  throwHttpError err400 "Failed to parse RSA Public key."
keyErrorToHttpError (InvalidRSAKeySizeBRE (Expected (Bit expSize)) (Received (Bit recSize))) =
  throwHttpError err400 (BSL8.pack $ printf "Invalid RSA Key size. Expected: %d Bits, Received: %d Bits\n" expSize recSize)
keyErrorToHttpError KeyAlreadyRevokedBRE =
  throwHttpError err400 "Public key already revoked."
keyErrorToHttpError KeyAlreadyExpiredBRE =
  throwHttpError err400 "Public key already expired."
keyErrorToHttpError UnauthorisedKeyAccessBRE =
  throwHttpError err403 "Not authorised to access this key."
keyErrorToHttpError (PublicKeyInsertionErrorBRE _) =
  throwHttpError err500 "Public key could not be inserted."
keyErrorToHttpError (KeyNotFoundBRE _) =
  throwHttpError err404 "Public key with the given id not found."
keyErrorToHttpError (InvalidRevocationBRE _ _ _) =
  throwHttpError err500 "Key has been revoked but in an invalid way."
keyErrorToHttpError (AddedExpiredKeyBRE) =
  throwHttpError err400 "Can't add a key that has already expired."
