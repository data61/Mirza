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
{-# LANGUAGE UndecidableInstances  #-}

-- | Endpoint definitions go here. Most of the endpoint definitions are
-- light wrappers around functions in BeamQueries
module Mirza.OrgRegistry.Service
  ( appHandlers
  , publicServer
  , privateServer
  , appMToHandler
  , serveSwaggerAPI
  , module Handlers
  ) where

import           Mirza.Common.Utils

import           Mirza.OrgRegistry.API

import           Mirza.OrgRegistry.Auth              (oauthClaimsToAuthUser)
import           Mirza.OrgRegistry.Handlers.Health   as Handlers
import           Mirza.OrgRegistry.Handlers.Keys     as Handlers
import           Mirza.OrgRegistry.Handlers.Location as Handlers
import           Mirza.OrgRegistry.Handlers.Org      as Handlers
import           Mirza.OrgRegistry.Handlers.Users    as Handlers
import           Mirza.OrgRegistry.Types

import           Katip

import           Servant
import           Servant.Auth.Server
import           Servant.Swagger

import           Control.Lens                        hiding ((.=))
import           Control.Monad                       ((<=<))
import           Control.Monad.IO.Class              (liftIO)
import           Control.Monad.Trans

import           Data.ByteString.Lazy.Char8          as BSL8
import qualified Data.HashMap.Strict.InsOrd          as IOrd
import           Text.Printf                         (printf)

import           Data.Swagger


-- Convenience class for contexts which require all possible error types that
-- could be thrown through the handlers.
class (AsORKeyError err, AsORError err, AsSqlError err)
  => APIPossibleErrors err where
instance (AsORKeyError err, AsORError err, AsSqlError err)
  => APIPossibleErrors err


appHandlers :: ( Member context '[HasDB]
               , APIPossibleErrors err)
            => ServerT ServerAPI (AppM context err)
appHandlers = publicServer :<|> privateServer

publicServer :: ( Member context '[HasDB]
                , APIPossibleErrors err)
             => ServerT PublicAPI (AppM context err)
publicServer =
       health
  :<|> versionInfo
  :<|> getPublicKeyInfo
  :<|> getPublicKey
  :<|> searchOrgs
  :<|> getLocationByGLN
  :<|> searchLocation
  :<|> searchOrgLocation
  :<|> searchOrgLocationByGLN

privateServer :: ( Member context '[HasDB]
                 , APIPossibleErrors err)
              => ServerT ProtectedAPI (AppM context err)
privateServer =      (transformUser0 addUserAuth)
                :<|> (transformUser0 getOrgInfo)
                :<|> (transformUser2 addOrgAuth)
                :<|> (transformUser2 addOrgMappingAuth)
                :<|> (transformUser3 addPublicKey)
                :<|> (transformUser1 revokePublicKey)
                :<|> (transformUser1 addLocation)
  where
    -- Because decodeJWT is pure we can't properly transform our user which requires acess the database and the ability
    -- to fail using our error types. It would be nice to apply oauthClaimsToAuthUser uniformly to all endpoints, but
    -- currently the following method is the cleanest way that we know of applying it to each of the end points. The
    -- number is the number of argument that the end point takes.
    transformUser0 f              = f <=< oauthClaimsToAuthUser
    transformUser1 f claims a     = (\user -> f user a)     =<< (oauthClaimsToAuthUser claims)
    transformUser2 f claims a b   = (\user -> f user a b)   =<< (oauthClaimsToAuthUser claims)
    transformUser3 f claims a b c = (\user -> f user a b c) =<< (oauthClaimsToAuthUser claims)

instance (HasSwagger sub) => HasSwagger (Servant.Auth.Server.Auth '[JWT] a :> sub) where
  toSwagger _ =
    let
      method = "OAuth2"
      securityRequirements = [SecurityRequirement $ IOrd.singleton method []]
      -- Using the proper OAuth implementation doesn't work because the redirect is broken. Leaving this here for if the
      -- issue gets fixed. The relevant issue is: https://github.com/haskell-servant/servant-swagger-ui/issues/54
      --oauth2params = OAuth2Params (OAuth2Implicit " https://mirza.au.auth0.com/authorize") IOrd.empty
      --authSchemes = IOrd.singleton method $ SecurityScheme (SecuritySchemeOAuth2 oauth2params) Nothing
      authDescription = "Use the format 'Bearer [Token]' to supply the token to the Authorization field."
      authSchemes = IOrd.singleton method $ SecurityScheme (SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader)) (Just authDescription)
    in
      toSwagger (Proxy :: Proxy sub)
      & allOperations . security .~ securityRequirements
      & securityDefinitions .~ authSchemes


appMToHandler :: (HasLogging context) => context -> AppM context ORError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ runAppM context act
  case res of
    Left err ->
      runKatipContextT (context ^. katipLogEnv) () (context ^. katipNamespace) (orErrorToHttpError err)
    Right a  -> pure a


-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Org Registry Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("Apache-2.0" & url ?~ URL "https://www.apache.org/licenses/LICENSE-2.0.txt")


errorLogLevel :: ServantErr -> Severity
errorLogLevel httpStatus
  | is5XXError(httpStatus) = ErrorS
  | otherwise = WarningS

-- | Is the servant error in the 5XX series?
is5XXError :: ServantErr -> Bool
is5XXError servantError = ((errHTTPCode servantError) `div` 100) == 5


-- | This function simplifies the construction of errors by providing an
-- interface with just the arguments necessary. This function logs the error
-- and if the the status code is in the 5XX the log level is escalated. We log
-- all errors for now so that developers have the oppertunity to skim the logs
-- to look for potential issues. The error type contains all the information
-- that we know about the error at this point so we add it in entirity to the
-- log.
-- TODO: Transform Show error so that we can only log OR and ORKeyErrors to
-- further constrain the type and prevent accidental errors in the argument
-- provided, even though all we need is show.
throwHttpError :: (Show error) => error -> ServantErr -> ByteString -> KatipContextT Handler a
throwHttpError err httpStatus errorMessage = do
  $(logTM) (errorLogLevel httpStatus) (logStr $ show err)
  lift $ throwError $ httpStatus { errBody = errorMessage }


-- | Takes a ORError and converts it to an HTTP error.
orErrorToHttpError :: ORError -> KatipContextT Handler a
orErrorToHttpError orError =
  let httpError = throwHttpError orError
  in case orError of
    (ORKeyErrorORE keyError)        -> orKeyErrorToHttpError keyError
    (DBErrorORE _)                  -> unexpectedError orError
    (UnexpectedErrorORE _)          -> unexpectedError orError
    (UnmatchedUniqueViolationORE _) -> unexpectedError orError
    (LocationNotKnownORE)           -> httpError err404 "Unknown GLN"
    (LocationExistsORE)             -> httpError err409 "GLN already exists"
    (GS1CompanyPrefixExistsORE)     -> httpError err400 "GS1 company prefix already exists."
    (OrgDoesNotExistORE)       -> httpError err400 "Organisation does not exist."
    (OperationNotPermittedORE _ _)  -> httpError err403 "A user can only act on behalf of the org they are associated with."
    (UserAuthFailureORE _)          -> httpError err401 "Authorization invalid."
    (InvalidOAuthSubORE)            -> httpError err400 "Invalid OAuth Sub."
    (UserCreationErrorORE _ _)      -> userCreationError orError
    UnknownUserORE                  -> httpError err400 "Unknown User"

-- | A generic internal server error has occured. We include no more information in the result returned to the user to
-- limit further potential for exploitation, under the expectation that we log the errors to somewhere that is reviewed
-- regularly so that the development team are informed and can identify and patch the underlying issues.
unexpectedError :: ORError -> KatipContextT Handler a
unexpectedError orError = throwHttpError orError err500 "An unknown error has occured."

-- | A common function for handling user errors uniformly irrespective of what the underlying cause is.
userCreationError :: ORError -> KatipContextT Handler a
userCreationError orError = throwHttpError orError err400 "Unable to create user."


-- | Takes a ORKeyError and converts it to an HTTP error.
orKeyErrorToHttpError :: ORKeyError -> KatipContextT Handler a
orKeyErrorToHttpError keyError =
  let httpError = throwHttpError keyError
  in case keyError of
    (InvalidRSAKeyORKE _)           -> httpError err400 "Failed to parse RSA Public key."
    KeyAlreadyRevokedORKE           -> httpError err400 "Public key already revoked."
    KeyAlreadyExpiredORKE           -> httpError err400 "Public key already expired."
    UnauthorisedKeyAccessORKE       -> httpError err403 "Not authorised to access this key."
    (PublicKeyInsertionErrorORKE _) -> httpError err500 "Public key could not be inserted."
    (KeyNotFoundORKE _)             -> httpError err404 "Public key with the given id not found."
    (InvalidRevocationORKE{})       -> httpError err500 "Key has been revoked but in an invalid way."
    (AddedExpiredKeyORKE)           -> httpError err400 "Can't add a key that has already expired."
    (InvalidRSAKeySizeORKE (Expected (Bit expSize)) (Received (Bit recSize)))
                                    -> httpError err400 (BSL8.pack $ printf "Invalid RSA Key size. Expected: %d Bits, Received: %d Bits\n" expSize recSize)
    (KeyIsPrivateKeyORKE)           -> httpError err400 "WARNING! Submitted Key was a Private Key, you should no longer continue to use it!"
