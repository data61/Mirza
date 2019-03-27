{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.BusinessRegistry.Auth
  (
    basicAuthServerContext
  , listUsersQuery
  ) where

import           Mirza.BusinessRegistry.Database.Schema as Schema
import           Mirza.BusinessRegistry.Types           as BT
import           Mirza.Common.Types                     as CT

import           Database.Beam                          as B
import           Database.PostgreSQL.Simple             (SqlError)

import           Servant

import qualified Crypto.Scrypt                          as Scrypt

import           Control.Lens

import           Text.Email.Validate                    (EmailAddress,
                                                         emailAddress)

import Servant.Auth.Server


import Debug.Trace

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
-- todo change the name of this function.
basicAuthServerContext :: ( Member context '[HasScryptParams, HasDB, BT.HasTheJWK])
                       => context -> Servant.Context '[JWTSettings, CookieSettings]
basicAuthServerContext context = defaultJWTSettings (trace ("JWK:" <> (show (view givemethejwk context))) (view givemethejwk context )) :. defaultCookieSettings :. EmptyContext


userToAuthUser :: Schema.User -> AuthUser
userToAuthUser user = AuthUser (CT.UserId $ user_id user)


listUsersQuery :: DB context err [Schema.User]
listUsersQuery = pg $ runSelectReturningList $ select $
    all_ (_users businessRegistryDB)
