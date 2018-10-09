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
  $(logTM) WarningS (logStr $ show brError)
  brErrorToHttpError brError


-- | This function simplifies the construction of errors by providing an
-- interface with just the arguments necessary. This function logs the error if
-- the status code is 500.
-- TODO: Transform Show error so that we can only log BR and KeyErrors to
-- further constrain the type and prevent accidental errors in the argument
-- provided, even though all we need is show.
throwHttpError :: (Show error) => ServantErr -> ByteString -> error -> KatipContextT Handler a
throwHttpError httpStatus errorMessage brError
  | is500Error(httpStatus) = logThrowHttpError httpStatus errorMessage brError
  | otherwise = throwHttpError' httpStatus errorMessage

-- | Is the servant error in the 5XX series?
is500Error :: ServantErr -> Bool
is500Error servantError = ((errHTTPCode servantError) `div` 100) == 5


-- TODO: Transform Show error so that we can only log BR and KeyErrors to
-- further constrain the type and prevent accidental errors in the argument
-- provided, even though all we need is show.
logThrowHttpError :: (Show error) => ServantErr -> ByteString -> error -> KatipContextT Handler a
logThrowHttpError httpStatus errorMessage err = do
  $(logTM) ErrorS (logStr $ show err)
  throwHttpError' httpStatus errorMessage

throwHttpError' :: ServantErr -> ByteString -> KatipContextT Handler a
throwHttpError' httpStatus errorMessage = lift $ throwError $ httpStatus { errBody = errorMessage }


-- | Takes in a BusinessRegistryError and converts it to an HTTP error (eg. err400)
brErrorToHttpError :: BusinessRegistryError -> KatipContextT Handler a
brErrorToHttpError err =
  let httpError = (\x y -> throwHttpError x y err)
  in case err of
    (KeyErrorBRE keyError)           -> keyErrorToHttpError keyError
    (DBErrorBRE _)                  -> unexpectedError err
    (UnexpectedErrorBRE _)          -> unexpectedError err
    (UnmatchedUniqueViolationBRE _) -> unexpectedError err
    (LocationNotKnownBRE)           -> httpError err404 "Unknown GLN"
    (LocationExistsBRE)             -> httpError err409 "Location already exists for this GLN"
    (GS1CompanyPrefixExistsBRE)     -> httpError err400 "GS1 company prefix already exists."
    (BusinessDoesNotExistBRE)       -> httpError err400 "Business does not exist."
    (UserCreationErrorBRE _ _)      -> userCreationError err
    (UserCreationSQLErrorBRE _)     -> userCreationError err

-- | A generic internal server error has occured. We include no more information in the result returned to the user to
-- limit further potential for exploitation, under the expectation that we log the errors to somewhere that is reviewed
-- regularly so that the development team are informed and can identify and patch the underlying issues.
unexpectedError :: BusinessRegistryError -> KatipContextT Handler a
unexpectedError = throwHttpError err500 "An unknown error has occured."

-- | A common function for handling user errors uniformly irrespective of what the underlying cause is.
userCreationError :: BusinessRegistryError -> KatipContextT Handler a
userCreationError = throwHttpError err400 "Unable to create user."


keyErrorToHttpError :: KeyError -> KatipContextT Handler a
keyErrorToHttpError err =
  let httpError = (\x y -> throwHttpError x y err)
  in case err of
    (InvalidRSAKeyBRE _)           -> httpError err400 "Failed to parse RSA Public key."
    KeyAlreadyRevokedBRE           -> httpError err400 "Public key already revoked."
    KeyAlreadyExpiredBRE           -> httpError err400 "Public key already expired."
    UnauthorisedKeyAccessBRE       -> httpError err403 "Not authorised to access this key."
    (PublicKeyInsertionErrorBRE _) -> httpError err500 "Public key could not be inserted."
    (KeyNotFoundBRE _)             -> httpError err404 "Public key with the given id not found."
    (InvalidRevocationBRE _ _ _)   -> httpError err500 "Key has been revoked but in an invalid way."
    (AddedExpiredKeyBRE)           -> httpError err400 "Can't add a key that has already expired."
    (InvalidRSAKeySizeBRE (Expected (Bit expSize)) (Received (Bit recSize))) -> httpError err400 (BSL8.pack $ printf "Invalid RSA Key size. Expected: %d Bits, Received: %d Bits\n" expSize recSize)
