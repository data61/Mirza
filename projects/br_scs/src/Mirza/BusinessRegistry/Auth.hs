{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.BusinessRegistry.Auth
  (
    tokenServerContext
  , tableUserToAuthUser
  , listUsersQuery
  , oauthClaimsToAuthUser
  , getUserByOAuthSubQuery
  , userOrganisationAutherisation
  , checkUserExistsQuery
  ) where

import           Mirza.BusinessRegistry.Database.Schema as Schema
import           Mirza.BusinessRegistry.Handlers.Users  as BU
import           Mirza.BusinessRegistry.Types           as BT
import           Mirza.Common.Types                     as CT

import           Data.GS1.EPC                           as EPC

import           Database.Beam                          as B

import           Servant
import           Servant.Auth.Server

import           Control.Lens                           hiding (mapping)

import           Data.Text                              (Text)
import           Data.Functor                           (void)
import           Data.Maybe                             (isNothing)
import           Control.Monad                          (when)

import           Crypto.JWT                              (Audience (..))


--------------------------------------------------------------------------------
-- Authentication
--------------------------------------------------------------------------------

-- | We need to supply our handlers with the right Context. In this case,
-- JWT requires a Context Entry with the 'JWTSettings and CookiesSettings values.
tokenServerContext :: ( Member context '[HasDB, HasAuthAudience, HasAuthPublicKey])
                       => context -> Servant.Context '[JWTSettings, CookieSettings]
tokenServerContext context = jwtSettings :. defaultCookieSettings :. EmptyContext
  where
    defaultSettings = defaultJWTSettings (view authPublicKey context)
    Audience audienceList = view authAudience context
    matchAudience aud = if (elem aud audienceList) then Matches else DoesNotMatch
    jwtSettings = defaultSettings {audienceMatches = matchAudience}


-- | Converts a DB representation of ``User`` to ``AuthUser``
tableUserToAuthUser :: Schema.User -> AuthUser
tableUserToAuthUser user = AuthUser (CT.UserId $ Schema.user_id user)


listUsersQuery :: DB context err [Schema.User]
listUsersQuery = pg $ runSelectReturningList $ select $
    all_ (_users businessRegistryDB)


oauthClaimsToAuthUser :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
                       , Member err     '[AsBRError, AsSqlError])
                    => Servant.Auth.Server.AuthResult BT.VerifiedTokenClaims
                    -> AppM context err BT.AuthUser
oauthClaimsToAuthUser (Authenticated claims) = do
  maybeUser <- runDb (getUserByOAuthSubQuery $ verifiedTokenClaimsSub claims)
  case maybeUser of
    Just user -> pure $ tableUserToAuthUser user
    Nothing   -> tableUserToAuthUser <$> (addUser promotedUser)
  where
    promotedUser = NewUser (verifiedTokenClaimsSub claims)
oauthClaimsToAuthUser failure = throwing _UserAuthFailureBRE (void failure)


getUserByOAuthSubQuery :: Text -> DB context err (Maybe Schema.User)
getUserByOAuthSubQuery oauthSub = do
  r <- pg $ runSelectReturningList $ select $ do
          user <- all_ (Schema._users Schema.businessRegistryDB)
          guard_ (user_oauth_sub user ==. val_ oauthSub)
          pure user
  case r of
    [user] -> pure $ Just user
    _      -> pure Nothing


--------------------------------------------------------------------------------
-- Authorisation
--------------------------------------------------------------------------------

userOrganisationAutherisation :: ( Member context '[]
                                 , Member err     '[AsBRError])
                              => AuthUser
                              -> GS1CompanyPrefix
                              -> DB context err OrganisationMapping
userOrganisationAutherisation (AuthUser (BT.UserId uId)) gs1CompantPrefix = do
  maybeMapping <- pg $ runSelectReturningOne $ select $ do
    mapping <- all_ (_organisationMapping businessRegistryDB)
    guard_ (organisation_mapping_user_id mapping ==. val_ (Schema.UserId uId))
    guard_ (organisation_mapping_gs1_company_prefix mapping ==. val_ (BizId gs1CompantPrefix))
    pure $ mapping
  case maybeMapping of
    Nothing -> throwing _OperationNotPermittedBRE (gs1CompantPrefix, BT.UserId uId)
    Just mapping -> pure mapping




-- This doesn't really belong anywhere atm, so for now it can go here, but can
-- be moved somewhere better when a suitable location is found.
checkUserExistsQuery :: (AsBRError err)
                => BT.UserId -> DB context err ()
checkUserExistsQuery userId = do
  user <- pg $ runSelectReturningOne $ select $ do
    user <- all_ (Schema._users Schema.businessRegistryDB)
    guard_ (user_id user ==. val_ (getUserId userId))
    pure user
  when (isNothing user) $ throwing_ _UserDoesNotExistBRE
  pure ()