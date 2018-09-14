{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.BusinessRegistry.Handlers.Users
  (
    addUser
  , addUserAuth
  , addUserQuery
  , getUserByIdQuery
  , tableToAuthUser
  ) where

import           Mirza.BusinessRegistry.Database.Schema   hiding (UserId)
import qualified Mirza.BusinessRegistry.Database.Schema   as Schema

import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types             as BRT
import           Mirza.Common.Utils

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import qualified Crypto.Scrypt                            as Scrypt

import           Control.Lens                             (view, _2)
import           Control.Monad                            (when)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Maybe                               (isNothing)
import           Data.Text.Encoding                       (encodeUtf8)



-- This function is an interface adapter and adds the BT.AuthUser argument to
-- addUser so that we can use it from behind the private API. This argument
-- is not used in the current implementation as it is assumed that all users
-- will have the ability to act globally.
addUserAuth ::  (BRApp context err, HasScryptParams context)
        => AuthUser
        -> NewUser
        -> AppM context err UserId
addUserAuth _authUser = addUser

addUser ::  (BRApp context err, HasScryptParams context)
        => NewUser
        -> AppM context err UserId
addUser = runDb . addUserQuery

-- | Hashes the password of the NewUser and inserts the user into the database
addUserQuery :: (AsBusinessRegistryError err, HasScryptParams context)
             => NewUser
             -> DB context err UserId
addUserQuery (NewUser (EmailAddress email) password companyPrefix firstName lastName phone) = do
  params <- view $ _2 . scryptParams
  encPass <- liftIO $ Scrypt.encryptPassIO params (Scrypt.Pass $ encodeUtf8 password)
  userId <- newUUID

  business <- pg $ runSelectReturningOne $ select $ do
    businesses <- all_ (Schema._businesses Schema.businessRegistryDB)
    guard_ (biz_gs1_company_prefix businesses ==. val_ companyPrefix)
    pure businesses
  when (isNothing business) $ throwing_ _BusinessDoesNotExistBRE

  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- pg $ runInsertReturningList (Schema._users Schema.businessRegistryDB) $
      insertValues
       [Schema.UserT userId (Schema.BizId companyPrefix) firstName lastName
               phone (Scrypt.getEncryptedPass encPass) email
       ]
  case res of
      [r] -> return $ UserId $ user_id r
      -- TODO: Have a proper error response
      _   -> throwing _UserCreationErrorBRE (show res)


getUserByIdQuery :: UserId -> DB context err (Maybe Schema.User)
getUserByIdQuery (UserId uid) = do
  r <- pg $ runSelectReturningList $ select $ do
          user <- all_ (Schema._users Schema.businessRegistryDB)
          guard_ (user_id user ==. val_ uid)
          pure user
  case r of
    [user] -> return $ Just user
    _      -> return Nothing


-- | Converts a DB representation of ``User`` to ``AuthUser``
tableToAuthUser :: Schema.User -> AuthUser
tableToAuthUser tUser = AuthUser (UserId $ Schema.user_id tUser)