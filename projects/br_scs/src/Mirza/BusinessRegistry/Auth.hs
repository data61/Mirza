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
  ) where

import           Mirza.BusinessRegistry.Database.Schema as Schema
import           Mirza.BusinessRegistry.Handlers.Users  as BU
import           Mirza.BusinessRegistry.Types           as BT
import           Mirza.Common.Types                     as CT

import           Data.GS1.EPC                           as EPC

import           Database.Beam                          as B

import           Servant
import           Servant.Auth.Server

import           Control.Lens

import           Text.Email.Validate                    (unsafeEmailAddress)

import           Data.Text                              (Text)
import           Data.Functor                           (void)

import           Crypto.JWT                              (Audience (..))


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
    -- TODO: Prmoted user is a hack, this allows us to update the auth process without changing to much of the
    --       user structure. For this to work there needs to be a company with the GS1CompanyPrefix "bootstrap"
    --       as it is an assumption of this code. In phase 2 of this implementation we will come back and remove
    --       all of the additional user metadata which is no longer needed.
    promotedUser = NewUser (verifiedTokenClaimsSub claims) (unsafeEmailAddress "promoted-user" "example.com") (GS1CompanyPrefix "bootstrap") "" "" ""
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
