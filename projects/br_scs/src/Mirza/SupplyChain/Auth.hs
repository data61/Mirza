{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.SupplyChain.Auth where

import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.ErrorUtils      (throwAppError,
                                                    throwBackendError)
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.Types           hiding (NewUser (..),
                                                    User (userId), UserId)
import qualified Mirza.SupplyChain.Types           as ST

import           Database.Beam                     as B

import           Servant

import qualified Crypto.Scrypt                     as Scrypt

import           Control.Lens                      (view, _2)

import           Text.Email.Validate               (EmailAddress, emailAddress)


-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
-- basicAuthServerContext :: (HasScryptParams context, DBConstraint context ServiceError)
--                        => context  -> Servant.Context '[BasicAuthCheck ST.User]
-- basicAuthServerContext context = authCheck context :. EmptyContext


-- -- 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
-- -- authCheck :: SCSContext -> BasicAuthCheck ST.User
-- authCheck :: (HasScryptParams context, DBConstraint context ServiceError)
--           => context -> BasicAuthCheck ST.User
-- authCheck context =
--   let check (BasicAuthData userEmail pass) =
--         case emailAddress userEmail of
--           Nothing -> pure Unauthorized
--           Just email -> do
--             eitherUser <- runAppM @_ @ServiceError context . runDb $
--                           authCheckQuery email (Password pass)
--             case eitherUser of
--               Right (Just user) -> pure (Authorized user)
--               _                 -> pure Unauthorized
--   in BasicAuthCheck check


-- -- Basic Auth check using Scrypt hashes.
-- -- TODO: How safe is this to timing attacks? Can we tell which emails are in the
-- -- system easily?
-- authCheckQuery :: (AsServiceError err, HasScryptParams context)
--                =>  EmailAddress
--                -> Password
--                -> DB context err (Maybe ST.User)
-- authCheckQuery userEmail (Password password) = do
--   r <- pg $ runSelectReturningList $ select $ do
--         user <- all_ (Schema._users Schema.supplyChainDb)
--         guard_ (Schema.user_email_address user  ==. val_ userEmail)
--         pure user
--   params <- view $ _2 . scryptParams
--   case r of
--     [user] ->
--         case Scrypt.verifyPass params (Scrypt.Pass password)
--               (Scrypt.EncryptedPass $ Schema.user_password_hash user)
--         of
--           (False, _     ) -> throwAppError $ AuthFailed userEmail
--           (True, Nothing) -> pure $ Just (userTableToModel user)
--           (True, Just (Scrypt.EncryptedPass password')) -> do
--             _ <- pg $ runUpdate $ update (Schema._users Schema.supplyChainDb)
--                     (\u -> [Schema.user_password_hash u <-. val_ password'])
--                     (\u -> Schema.user_id u ==. val_ (Schema.user_id user))
--             pure $ Just (userTableToModel user)
--     [] -> throwAppError $ UserNotFound userEmail
--     _  -> throwBackendError r -- multiple elements
