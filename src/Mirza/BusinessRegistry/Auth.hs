{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.BusinessRegistry.Auth
  (
    basicAuthServerContext
  , authCheck
  , listUsersQuery
  ) where

import           Database.Beam                            as B
import           Mirza.BusinessRegistry.Database.Schema   as Schema
import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.Common.Utils

import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.PostgreSQL.Simple               (SqlError)

import           Data.Text.Encoding                       (decodeUtf8)

import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Types                       as CT

import           Servant

import qualified Crypto.Scrypt                            as Scrypt

import           Control.Lens


-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: (HasScryptParams context, DBConstraint context BusinessRegistryError)
                       => context  -> Servant.Context '[BasicAuthCheck AuthUser]
basicAuthServerContext context = authCheck context :. EmptyContext


-- 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
-- authCheck :: SCSContext -> BasicAuthCheck ST.User
authCheck :: (HasScryptParams context, DBConstraint context SqlError)
          => context -> BasicAuthCheck AuthUser
authCheck context =
  let check (BasicAuthData useremail pass) = do
        eitherUser <- runAppM @_ @SqlError context . runDb $
                      authCheckQuery (EmailAddress $ decodeUtf8 useremail) (Password pass)
        case eitherUser of
          Right (Just user) -> return (Authorized user)
          _                 -> return Unauthorized
  in BasicAuthCheck check


-- Basic Auth check using Scrypt hashes.
-- TODO: How safe is this to timing attacks? Can we tell which emails are in the
-- system easily?
authCheckQuery :: (AsSqlError err, HasScryptParams context) =>  EmailAddress -> Password -> DB context err (Maybe AuthUser)
authCheckQuery (EmailAddress email) (Password password) = do
  let userTable = _users businessRegistryDB
  r <- pg $ runSelectReturningList $ select $ do
        user <- all_ userTable
        guard_ (email_address user  ==. val_ email)
        pure user
  params <- view $ _2 . scryptParams
  case r of
    [user] ->
        case Scrypt.verifyPass params (Scrypt.Pass password)
              (Scrypt.EncryptedPass $ password_hash user)
        of
          (False, _     ) -> pure Nothing
          (True, Nothing) -> pure $ Just (userToAuthUser user)
          (True, Just (Scrypt.EncryptedPass password')) -> do
            _ <- pg $ runUpdate $ update userTable
                    (\u -> [password_hash u <-. val_ password'])
                    (\u -> user_id u ==. val_ (user_id user))
            pure $ Just (userToAuthUser user)
    _ -> pure Nothing


userToAuthUser :: Schema.User -> AuthUser
userToAuthUser user = AuthUser (CT.UserId $ user_id user)


listUsersQuery :: BRApp context err => DB context err [Schema.User]
listUsersQuery = pg $ runSelectReturningList $ select $
    all_ (_users businessRegistryDB)
