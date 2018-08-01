{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.BusinessRegistry.Handlers.Users
  (
    newUser
  , getUserByIdQuery
  , tableToAuthUser
  ) where

import           Mirza.BusinessRegistry.Database.Schema   hiding (UserID)
import           Mirza.BusinessRegistry.Database.Schema   as Schema

import           Mirza.BusinessRegistry.Handlers.Common
import qualified Mirza.BusinessRegistry.Types             as BRT
import           Mirza.Common.Utils

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import qualified Crypto.Scrypt                            as Scrypt

import           Control.Lens                             (view, _2)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Text.Encoding                       (encodeUtf8)



newUser ::  (BRApp context err, BRT.HasScryptParams context) => BRT.NewUser
        -> BRT.AppM context err BRT.UserID
newUser = BRT.runDb . newUserQuery


-- | Hashes the password of the BRT.NewUser and inserts the user into the database
newUserQuery :: (BRT.AsBusinessRegistryError err, BRT.HasScryptParams context) => BRT.NewUser
             -> BRT.DB context err BRT.UserID
newUserQuery userInfo@(BRT.NewUser _ _ _ _ _ password) = do
  params <- view $ _2 . BRT.scryptParams
  hash <- liftIO $ Scrypt.encryptPassIO params (Scrypt.Pass $ encodeUtf8 password)
  insertUser hash userInfo


insertUser :: BRT.AsBusinessRegistryError err => Scrypt.EncryptedPass
           -> BRT.NewUser
           -> BRT.DB context err BRT.UserID
insertUser encPass (BRT.NewUser phone (BRT.EmailAddress email) firstName lastName biz _) = do
  userId <- newUUID
  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- BRT.pg $ runInsertReturningList (Schema._users Schema.businessRegistryDB) $
      insertValues
       [Schema.UserT userId (Schema.BizId  biz) firstName lastName
               phone (Scrypt.getEncryptedPass encPass) email
        ]
  case res of
      [r] -> return $ BRT.UserID $ user_id r
      -- TODO: Have a proper error response
      _   -> BRT.throwing BRT._UserCreationErrorBRE (show res)


getUserByIdQuery :: BRT.UserID -> BRT.DB context err (Maybe Schema.User)
getUserByIdQuery (BRT.UserID uid) = do
  r <- BRT.pg $ runSelectReturningList $ select $ do
          user <- all_ (Schema._users Schema.businessRegistryDB)
          guard_ (user_id user ==. val_ uid)
          pure user
  case r of
    [user] -> return $ Just user
    _      -> return Nothing


-- | Converts a BRT.DB representation of ``User`` to ``AuthUser``
tableToAuthUser :: Schema.User -> BRT.AuthUser
tableToAuthUser tUser = BRT.AuthUser (BRT.UserID $ Schema.user_id tUser)
