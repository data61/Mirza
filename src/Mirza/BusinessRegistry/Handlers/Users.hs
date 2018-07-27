{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.BusinessRegistry.Handlers.Users
  (
    newUser
  ) where

import           Mirza.BusinessRegistry.Database.Schema   hiding (UserID)
import           Mirza.BusinessRegistry.Database.Schema   as Schema

import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Utils

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (..),
                                                           constraintViolation)
import           Database.PostgreSQL.Simple.Internal      (SqlError (..))

import qualified Crypto.Scrypt                            as Scrypt

import           Control.Lens                             (view, (^?), _2)
import           Control.Monad.Except                     (MonadError,
                                                           throwError)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Text.Encoding                       (encodeUtf8)



newUser ::  (BRApp context err, HasScryptParams context)=> BT.NewUser -> AppM context err BT.UserID
newUser = runDb . newUserQuery


-- | Hashes the password of the BT.NewUser and inserts the user into the database
newUserQuery :: (AsBusinessRegistryError err, HasScryptParams context) => BT.NewUser -> DB context err BT.UserID
newUserQuery userInfo@(BT.NewUser _ _ _ _ _ password) = do
  params <- view $ _2 . scryptParams
  hash <- liftIO $ Scrypt.encryptPassIO params (Scrypt.Pass $ encodeUtf8 password)
  insertUser hash userInfo


insertUser :: AsBusinessRegistryError err => Scrypt.EncryptedPass
           -> BT.NewUser
           -> DB context err BT.UserID
insertUser encPass (BT.NewUser phone (EmailAddress email) firstName lastName biz _) = do
  userId <- newUUID
  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- --handleError errHandler $
    pg $ runInsertReturningList (Schema._users Schema.businessRegistryDB) $
      insertValues
       [Schema.UserT userId (Schema.BizId  biz) firstName lastName
               phone (Scrypt.getEncryptedPass encPass) email
        ]
  case res of
      [r] -> return $ BT.UserID $ user_id r
      -- TODO: Have a proper error response
      _   -> throwing _UserCreationErrorBRE (show res)
