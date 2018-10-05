{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Users
  (
    addUser, userTableToModel
  ) where


import           Mirza.Common.Utils
import           Mirza.SupplyChain.Database.Schema        as Schema
import           Mirza.SupplyChain.ErrorUtils             (throwBackendError,
                                                           toServerError)
import           Mirza.SupplyChain.Handlers.Common
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.Types                  hiding (NewUser (..),
                                                           User (userId))
import qualified Mirza.SupplyChain.Types                  as ST

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

addUser :: (SCSApp context err, HasScryptParams context)
        => ST.NewUser
        -> AppM context err ST.UserId
addUser = runDb . addUserQuery


-- | Hashes the password of the ST.NewUser and inserts the user into the database
addUserQuery :: (AsServiceError err, HasScryptParams context)
             => ST.NewUser
             -> DB context err ST.UserId
addUserQuery (ST.NewUser phone userEmail firstName lastName biz password) = do
  params <- view $ _2 . scryptParams
  encPass <- liftIO $ Scrypt.encryptPassIO params (Scrypt.Pass $ encodeUtf8 password)
  userId <- newUUID
  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- handleError errHandler $ pg $ runInsertReturningList (Schema._users Schema.supplyChainDb) $
    insertValues
      [Schema.User userId (Schema.BizId  biz) firstName lastName
               phone (Scrypt.getEncryptedPass encPass) userEmail
      ]
  case res of
        [r] -> return . ST.UserId . Schema.user_id $ r
        -- TODO: Have a proper error response
        _   -> throwBackendError res
  where
    errHandler :: (AsServiceError err, MonadError err m) => err -> m a
    errHandler e = case e ^? _DatabaseError of
      Nothing -> throwError e
      Just sqlErr -> case constraintViolation sqlErr of
        Just (UniqueViolation "user_email_address_key")
          -> throwing _EmailExists userEmail
        _ -> throwing _InsertionFail (toServerError (Just . sqlState) sqlErr, emailToText userEmail)
