{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.OrgRegistry.Handlers.Users
  (
    addUserAuth
  , addUser
  , addUserOnlyId
  , addUserQuery
  , getUserByIdQuery
  ) where

import           Mirza.Common.Utils
import           Mirza.OrgRegistry.Database.Schema        hiding (UserId)
import qualified Mirza.OrgRegistry.Database.Schema        as Schema
import           Mirza.OrgRegistry.Types                  as ORT

import           Servant.API                              (NoContent (..))

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           GHC.Stack                                (HasCallStack,
                                                           callStack)


-- Nothing needs to be done by this endpoint, because by the time that this
-- function is called the user will already be added to the users table (this
-- happens via `(transformUser0 addUserAuth)` which calls `oauthClaimsToAuthUser`
-- and adds the user to the database if not present). This could be achieved by
-- calling any of the other private endpoints, but we have made this endpoint so
-- that there is a well defined intuitive process for "regsitering" a user with
-- the system. This function also acts as a placeholder incase we ever want to
-- add any other associated metadata when adding a user to the system.
addUserAuth :: AuthUser -> AppM context err NoContent
addUserAuth _ = pure NoContent


addUserOnlyId :: ( Member context '[HasDB]
                 , Member err     '[AsORError, AsSqlError])
              => NewUser
              -> AppM context err UserId
addUserOnlyId user = (UserId . user_id) <$> (addUser user)


addUser :: ( Member context '[HasDB]
           , Member err     '[AsORError, AsSqlError])
        => NewUser
        -> AppM context err Schema.User
addUser = runDb . addUserQuery


-- | Inserts the user into the database.
addUserQuery :: (AsORError err, HasCallStack)
             => NewUser
             -> DB context err Schema.User
addUserQuery (ORT.NewUser oauthSub) = do
  userId <- newUUID
  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- pg $ runInsertReturningList $ insert (Schema._users Schema.orgRegistryDB) $
      insertValues [Schema.UserT userId oauthSub Nothing]
  case res of
      [r] -> pure $ r
      -- The user creation has failed, but we can't really think of what would lead to this case.
      _   -> throwing _UserCreationErrorORE ((show res), callStack)


getUserByIdQuery :: UserId -> DB context err (Maybe Schema.User)
getUserByIdQuery (UserId uid) = do
  r <- pg $ runSelectReturningList $ select $ do
          user <- all_ (Schema._users Schema.orgRegistryDB)
          guard_ (user_id user ==. val_ uid)
          pure user
  case r of
    [user] -> pure $ Just user
    _      -> pure Nothing

