{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}

module Mirza.SupplyChain.Handlers.Users
  (
    addUser, userTableToModel
  ) where


import           Mirza.Common.Utils
import           Mirza.SupplyChain.Database.Schema        as Schema
import           Mirza.SupplyChain.ErrorUtils             (throwBackendError)
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.SqlUtils
import           Mirza.SupplyChain.Types                  hiding (NewUser (..),
                                                           User (userId))
import qualified Mirza.SupplyChain.Types                  as ST

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import qualified Crypto.Scrypt                            as Scrypt

import           Control.Lens                             (view, ( # ), _2)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Text.Encoding                       (encodeUtf8)

addUser :: (Member context '[HasDB, HasScryptParams],
            Member err     '[AsServiceError, AsSqlError])
        => ST.NewUser
        -> AppM context err ST.UserId
addUser user =
    handleError
      (handleSqlUniqueViloation
        "users_user_email_address_key"
        (const $ _EmailExists # (ST.newUserEmailAddress user))
      )
  . runDb
  . addUserQuery
  $ user


-- | Hashes the password of the ST.NewUser and inserts the user into the database
addUserQuery :: (Member context '[HasDB, HasScryptParams],
                 Member err     '[AsServiceError])
             => ST.NewUser
             -> DB context err ST.UserId
addUserQuery (ST.NewUser phone userEmail firstName lastName biz password) = do
  params <- view $ _2 . scryptParams
  encPass <- liftIO $ Scrypt.encryptPassIO params (Scrypt.Pass $ encodeUtf8 password)
  userId <- newUUID
  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- pg $ runInsertReturningList (Schema._users Schema.supplyChainDb) $
    insertValues
      [Schema.User Nothing userId (Schema.BizId  biz) firstName lastName
               phone (Scrypt.getEncryptedPass encPass) userEmail
      ]
  case res of
        [r] -> pure . ST.UserId . Schema.user_id $ r
        -- TODO: Have a proper error response
        _   -> throwBackendError res
