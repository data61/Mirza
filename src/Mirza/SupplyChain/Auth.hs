{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.SupplyChain.Auth
  (
    basicAuthServerContext
  , authCheck
  ) where

import           Mirza.SupplyChain.ErrorUtils  (throwAppError,
                                                throwBackendError)
import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.StorageBeam as SB
import           Mirza.SupplyChain.Types       hiding (NewUser (..),
                                                User (userId), UserId)
import qualified Mirza.SupplyChain.Types       as ST

import           Database.Beam                 as B

import           Servant

import qualified Crypto.Scrypt                 as Scrypt

import           Control.Lens                  (view, _2)
import           Data.Text.Encoding            (decodeUtf8)


-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: (HasScryptParams context, DBConstraint context ServiceError)
                       => context  -> Servant.Context '[BasicAuthCheck ST.User]
basicAuthServerContext context = authCheck context :. EmptyContext


-- 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
-- authCheck :: SCSContext -> BasicAuthCheck ST.User
authCheck :: (HasScryptParams context, DBConstraint context ServiceError)
          => context -> BasicAuthCheck ST.User
authCheck context =
  let check (BasicAuthData useremail pass) = do
        eitherUser <- runAppM @_ @ServiceError context . runDb $
                      authCheckQuery (EmailAddress $ decodeUtf8 useremail) (Password pass)
        case eitherUser of
          Right (Just user) -> return (Authorized user)
          _                 -> return Unauthorized
  in BasicAuthCheck check


-- Basic Auth check using Scrypt hashes.
-- TODO: How safe is this to timing attacks? Can we tell which emails are in the
-- system easily?
authCheckQuery :: (AsServiceError err, HasScryptParams context) =>  EmailAddress -> Password -> DB context err (Maybe ST.User)
authCheckQuery e@(EmailAddress email) (Password password) = do
  r <- pg $ runSelectReturningList $ select $ do
        user <- all_ (SB._users SB.supplyChainDb)
        guard_ (SB.user_email_address user  ==. val_ email)
        pure user
  params <- view $ _2 . scryptParams
  case r of
    [user] ->
        case Scrypt.verifyPass params (Scrypt.Pass password)
              (Scrypt.EncryptedPass $ SB.user_password_hash user)
        of
          (False, _     ) -> throwAppError $ AuthFailed (EmailAddress email)
          (True, Nothing) -> pure $ Just (userTableToModel user)
          (True, Just (Scrypt.EncryptedPass password')) -> do
            _ <- pg $ runUpdate $ update (SB._users SB.supplyChainDb)
                    (\u -> [SB.user_password_hash u <-. val_ password'])
                    (\u -> SB.user_id u ==. val_ (SB.user_id user))
            pure $ Just (userTableToModel user)
    [] -> throwAppError $ EmailNotFound e
    _  -> throwBackendError r -- multiple elements
