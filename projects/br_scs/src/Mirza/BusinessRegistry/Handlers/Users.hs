{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}

module Mirza.BusinessRegistry.Handlers.Users
  (
    addUser
  , addUserOnlyId
  , addUserAuth
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

import qualified Crypto.Scrypt                            as Scrypt

import           Control.Lens                             ((#),view, _2)
import           Control.Monad                            (when)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Maybe                               (isNothing)
import           Data.Text.Encoding                       (encodeUtf8)

import           GHC.Stack                                (HasCallStack, callStack)


-- This function is an interface adapter and adds the BT.AuthUser argument to
-- addUser so that we can use it from behind the private API. This argument
-- is not used in the current implementation as it is assumed that all users
-- will have the ability to act globally.
addUserAuth :: ( Member context '[HasDB, HasScryptParams]
               , Member err     '[AsBRError, AsSqlError])
            => AuthUser
            -> NewUser
            -> AppM context err UserId
addUserAuth _authUser = addUserOnlyId


addUserOnlyId :: ( Member context '[HasDB, HasScryptParams]
           , Member err     '[AsBRError, AsSqlError])
        => NewUser
        -> AppM context err UserId
addUserOnlyId user = (UserId . user_id) <$> (addUser user)


addUser :: ( Member context '[HasDB, HasScryptParams]
           , Member err     '[AsBRError, AsSqlError])
        => NewUser
        -> AppM context err Schema.User
addUser =
  (handleError (handleSqlUniqueViloation "users_email_address_key" (_UserCreationSQLErrorBRE #)))
  . runDb
  . addUserQuery


-- | Hashes the password of the NewUser and inserts the user into the database
addUserQuery :: (HasScryptParams context, AsBRError err, HasCallStack)
             => NewUser
             -> DB context err Schema.User
addUserQuery (BRT.NewUser oauthSub userEmail password biz firstName lastName phone) = do
  params <- view $ _2 . scryptParams
  encPass <- liftIO $ Scrypt.encryptPassIO params (Scrypt.Pass $ encodeUtf8 password)
  userId <- newUUID

  business <- pg $ runSelectReturningOne $ select $ do
    businesses <- all_ (Schema._businesses Schema.businessRegistryDB)
    guard_ (biz_gs1_company_prefix businesses ==. val_ biz)
    pure businesses
  when (isNothing business) $ throwing_ _BusinessDoesNotExistBRE

  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- pg $ runInsertReturningList (Schema._users Schema.businessRegistryDB) $
      insertValues
       [Schema.UserT userId oauthSub (Schema.BizId  biz) firstName lastName
               phone (Scrypt.getEncryptedPass encPass) userEmail Nothing
       ]
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

