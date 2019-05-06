{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}

module Mirza.BusinessRegistry.Handlers.Users
  (
    addUser
  , addUserOnlyId
  , addUserQuery
  , getUserByIdQuery
  ) where

import           Mirza.BusinessRegistry.Database.Schema   hiding (UserId)
import qualified Mirza.BusinessRegistry.Database.Schema   as Schema
import           Mirza.BusinessRegistry.SqlUtils

import           Mirza.BusinessRegistry.Types             as BRT
import           Mirza.Common.Utils

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           Control.Lens                             ((#))
import           Control.Monad                            (when)
import           Data.Maybe                               (isNothing)

import           GHC.Stack                                (HasCallStack, callStack)


addUserOnlyId :: ( Member context '[HasDB]
           , Member err     '[AsBRError, AsSqlError])
        => NewUser
        -> AppM context err UserId
addUserOnlyId user = (UserId . user_id) <$> (addUser user)


addUser :: ( Member context '[HasDB]
           , Member err     '[AsBRError, AsSqlError])
        => NewUser
        -> AppM context err Schema.User
addUser =
  (handleError (handleSqlUniqueViloation "users_email_address_key" (_UserCreationSQLErrorBRE #)))
  . runDb
  . addUserQuery


-- | Hashes the password of the NewUser and inserts the user into the database
addUserQuery :: (AsBRError err, HasCallStack)
             => NewUser
             -> DB context err Schema.User
addUserQuery (BRT.NewUser oauthSub) = do
  userId <- newUUID
  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- pg $ runInsertReturningList (Schema._users Schema.businessRegistryDB) $
      insertValues
       [Schema.UserT userId oauthSub Nothing]
  case res of
      [r] -> pure $ r
      -- The user creation has failed, but we can't really think of what would lead to this case.
      _   -> throwing _UserCreationErrorBRE ((show res), callStack)


getUserByIdQuery :: UserId -> DB context err (Maybe Schema.User)
getUserByIdQuery (UserId uid) = do
  r <- pg $ runSelectReturningList $ select $ do
          user <- all_ (Schema._users Schema.businessRegistryDB)
          guard_ (user_id user ==. val_ uid)
          pure user
  case r of
    [user] -> pure $ Just user
    _      -> pure Nothing

