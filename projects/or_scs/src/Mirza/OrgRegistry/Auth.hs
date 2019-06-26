{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.OrgRegistry.Auth
  (
    tokenServerContext
  , tableUserToOAuthSub
  , tableUserToAuthUser
  , listUsersQuery
  , oauthClaimsToAuthUser
  , getUserByOAuthSubQuery
  , userOrganisationAuthorisationQuery
  , checkUserExistsQuery
  ) where

import           Mirza.Common.Types                as CT
import           Mirza.OrgRegistry.Database.Schema as Schema
import           Mirza.OrgRegistry.Handlers.Users  as BU
import           Mirza.OrgRegistry.Types           as ORT

import           Data.GS1.EPC                      as EPC

import           Database.Beam                     as B

import           Servant
import           Servant.Auth.Server

import           Control.Lens                      hiding (mapping)

import           Control.Monad                     (when)
import           Data.Functor                      (void)
import           Data.Maybe                        (isNothing)
import           Data.Text                         (Text)

import           Crypto.JWT                        (Audience (..))


--------------------------------------------------------------------------------
-- Authentication
--------------------------------------------------------------------------------

-- | We need to supply our handlers with the right Context. In this case,
-- JWT requires a Context Entry with the 'JWTSettings and CookiesSettings values.
tokenServerContext  :: ( Member context '[HasDB, HasAuthAudience, HasAuthPublicKey])
                    => context -> Servant.Context '[JWTSettings, CookieSettings]
tokenServerContext context = jwtSettings :. defaultCookieSettings :. EmptyContext
  where
    defaultSettings = defaultJWTSettings (view authPublicKey context)
    Audience audienceList = view authAudience context
    matchAudience aud = if (elem aud audienceList) then Matches else DoesNotMatch
    jwtSettings = defaultSettings {audienceMatches = matchAudience}

-- | Converts a DB representation of ``User`` to ``OAuthSub``
tableUserToOAuthSub :: Schema.User -> OAuthSub
tableUserToOAuthSub user = Schema.user_oauth_sub user

-- | Converts a DB representation of ``User`` to ``AuthUser``
tableUserToAuthUser :: Schema.User -> AuthUser
tableUserToAuthUser user = AuthUser $ Schema.user_oauth_sub user


listUsersQuery :: DB context err [Schema.User]
listUsersQuery = pg $ runSelectReturningList $ select $
    all_ (_users orgRegistryDB)


oauthClaimsToAuthUser :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                         , Member err     '[AsORError, AsSqlError])
                      => Servant.Auth.Server.AuthResult ORT.VerifiedTokenClaims
                      -> AppM context err ORT.AuthUser
oauthClaimsToAuthUser (Authenticated (ORT.VerifiedTokenClaims (OAuthSub ""))) = throwing_ _InvalidOAuthSubORE
oauthClaimsToAuthUser (Authenticated claims) = do
  maybeUser <- runDb (getUserByOAuthSubQuery $ verifiedTokenClaimsSub claims)
  case maybeUser of
    Just user -> pure $ tableUserToAuthUser user
    Nothing   -> tableUserToAuthUser <$> (addUser promotedUser)
  where
    promotedUser = NewUser (verifiedTokenClaimsSub claims)
oauthClaimsToAuthUser failure = throwing _UserAuthFailureORE (void failure)


getUserByOAuthSubQuery :: OAuthSub -> DB context err (Maybe Schema.User)
getUserByOAuthSubQuery oauthSub = do
  r <- pg $ runSelectReturningList $ select $ do
          user <- all_ (Schema._users Schema.orgRegistryDB)
          guard_ (user_oauth_sub user ==. val_ oauthSub)
          pure user
  case r of
    [user] -> pure $ Just user
    _      -> pure Nothing


--------------------------------------------------------------------------------
-- Authorisation
--------------------------------------------------------------------------------

userOrganisationAuthorisationQuery :: ( Member context '[]
                                      , Member err     '[AsORError])
                                   => AuthUser
                                   -> GS1CompanyPrefix
                                   -> DB context err OrgMapping
userOrganisationAuthorisationQuery (AuthUser oAuthSub) gs1CompantPrefix = do
  maybeMapping <- pg $ runSelectReturningOne $ select $ do
    mapping <- all_ (_orgMapping orgRegistryDB)
    guard_ (org_mapping_user_oauth_sub mapping ==. val_ (Schema.UserPrimaryKey oAuthSub))
    guard_ (org_mapping_gs1_company_prefix mapping ==. val_ (OrgPrimaryKey gs1CompantPrefix))
    pure $ mapping
  case maybeMapping of
    Nothing -> throwing _OperationNotPermittedORE (gs1CompantPrefix, oAuthSub)
    Just mapping -> pure mapping


-- This doesn't really belong anywhere atm, so for now it can go here, but can
-- be moved somewhere better when a suitable location is found.
checkUserExistsQuery :: (AsORError err)
                => OAuthSub -> DB context err ()
checkUserExistsQuery oAuthSub = do
  user <- pg $ runSelectReturningOne $ select $ do
    user <- all_ (Schema._users Schema.orgRegistryDB)
    guard_ (user_oauth_sub user ==. val_ oAuthSub)
    pure user
  when (isNothing user) $ throwing_ _UnknownUserORE
  pure ()
