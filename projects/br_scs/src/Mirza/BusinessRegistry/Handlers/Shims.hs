-- The functions in this file are a work around because we don't currently know
-- how to universally apply oauthClaimsToAuthUser before calling the handlers.
-- Once we work out how to apply oauthClaimsToAuthUser we should be able to
-- completely remove this file.


{-# LANGUAGE DataKinds             #-}

module Mirza.BusinessRegistry.Handlers.Shims
  ( addUserAuthShim
  , addBusinessAuthShim
  , getBusinessInfoShim
  , addPublicKeyShim
  , revokePublicKeyShim
  , addLocationShim
  ) where

import           Mirza.BusinessRegistry.Types
import           Mirza.BusinessRegistry.Database.Schema   as BS
import           Mirza.BusinessRegistry.Auth
import           Mirza.BusinessRegistry.Handlers.Business as Handlers
import           Mirza.BusinessRegistry.Handlers.Keys     as Handlers
import           Mirza.BusinessRegistry.Handlers.Location as Handlers
import           Mirza.BusinessRegistry.Handlers.Users    as Handlers
import           Mirza.Common.Time
import           Mirza.Common.Types                       as CT

import           Data.GS1.EPC                            (GS1CompanyPrefix (..))

import           Servant.Auth.Server

import           Crypto.JOSE                            (JWK)


addUserAuthShim :: ( Member context '[HasDB, HasScryptParams]
               , Member err     '[AsBRError, AsSqlError])
            => Servant.Auth.Server.AuthResult VerifiedTokenClaims
            -> NewUser
            -> AppM context err CT.UserId
addUserAuthShim claims user = do
                              auth <- oauthClaimsToAuthUser claims
                              addUserAuth auth user


addBusinessAuthShim :: ( Member context '[HasDB, HasScryptParams]
                   , Member err     '[AsBRError, AsSqlError])
                => Servant.Auth.Server.AuthResult VerifiedTokenClaims
                -> NewBusiness
                -> AppM context err GS1CompanyPrefix
addBusinessAuthShim claims business = do
                                      auth <- oauthClaimsToAuthUser claims
                                      addBusinessAuth auth business


getBusinessInfoShim :: ( Member context '[HasDB, HasScryptParams]
                   , Member err     '[AsBRError, AsSqlError])
                => Servant.Auth.Server.AuthResult VerifiedTokenClaims
                -> AppM context err [BusinessResponse]
getBusinessInfoShim claims = do
                             auth <- oauthClaimsToAuthUser claims
                             getBusinessInfo auth


addPublicKeyShim :: ( Member context '[HasEnvType, HasConnPool, HasLogging, HasScryptParams]
                , Member err     '[AsBRError, AsBRKeyError, AsSqlError])
             => Servant.Auth.Server.AuthResult VerifiedTokenClaims
             -> JWK
             -> Maybe ExpirationTime
             -> AppM context err BRKeyId
addPublicKeyShim claims jwk expirationTime = do
                                             auth <- oauthClaimsToAuthUser claims
                                             addPublicKey auth jwk expirationTime


revokePublicKeyShim :: ( Member context '[HasEnvType, HasConnPool, HasLogging, HasScryptParams]
                   , Member err     '[AsBRError, AsBRKeyError, AsSqlError])
                => Servant.Auth.Server.AuthResult VerifiedTokenClaims
                -> BRKeyId
                -> AppM context err RevocationTime
revokePublicKeyShim claims keyId = do
                                   auth <- oauthClaimsToAuthUser claims
                                   revokePublicKey auth keyId


addLocationShim :: ( Member context '[HasEnvType, HasConnPool, HasLogging, HasScryptParams]
               , Member err     '[AsBRError, AsSqlError])
            => Servant.Auth.Server.AuthResult VerifiedTokenClaims
            -> NewLocation
            -> AppM context err LocationId
addLocationShim claims location = do
                                  auth <- oauthClaimsToAuthUser claims
                                  addLocation auth location
