{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Mirza.BusinessRegistry.Handlers.Business
  (
    getPublicKey, getPublicKeyInfo
  , revokePublicKey
  , addPublicKey
  , listBusinesses
  , listBusinessesQuery
  , addBusinessQuery
  , addUserQuery
  ) where


import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.GS1BeamOrphans
import           Mirza.Common.Types
import           Mirza.Common.Utils


import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (..),
                                                           constraintViolation)

import           Data.Time.Clock                          (UTCTime)

import           Control.Monad.Except                     (MonadError,
                                                           throwError)



getPublicKey ::  BRApp context err => KeyID -> AppM context err PEM_RSAPubKey
getPublicKey = notImplemented



getPublicKeyInfo ::  BRApp context err => KeyID -> AppM context err BT.KeyInfo
getPublicKeyInfo = notImplemented


-- select * from Business;
listBusinesses :: BRApp context err => AppM context err [BusinessResponse]
listBusinesses = notImplemented


listBusinessesQuery :: BRApp context err => DB context err [Business]
listBusinessesQuery = pg $ runSelectReturningList $ select $
  all_ (_businesses businessRegistryDB)


addPublicKey :: BRApp context err => BT.AuthUser
             -> PEM_RSAPubKey
             -> Maybe ExpirationTime
             -> AppM context err KeyID
addPublicKey = notImplemented


revokePublicKey :: BRApp context err => BT.AuthUser -> KeyID -> AppM context err UTCTime
revokePublicKey = notImplemented

-- | Will _always_ create a new UUID for the BizId
addBusinessQuery :: BRApp context err => Business -> DB context err Business
addBusinessQuery biz'@BusinessT{..} = do
  -- The id is updated inside here to that it is generated as part of the
  -- transaction so if the transaction happens to fail because the UUID
  -- generated already exists it can be rerun in entirity hopefully with a
  -- better outcome.
  bizid <- newUUID
  let biz = biz'{business_id = bizid}

  res <- -- handleError errHandler $
         pg $ runInsertReturningList (_businesses businessRegistryDB) $
            insertValues [biz]
  case res of
        [r] -> return r
        -- TODO: Have a proper error response
        _   -> throwing _BusinessCreationError (show res)
  -- where
  --   errHandler :: (AsSqlError err, MonadError err m) => err -> m a
  --   errHandler e = case e ^? _DatabaseError of
  --     Nothing -> throwError e
  --     Just sqlErr -> case constraintViolation sqlErr of
  --       Just (UniqueViolation "users_email_address_key")
  --         -> throwing_ _BusinessExists
  --       _ -> throwing _InsertionFail (toServerError (Just . sqlState) sqlErr, email)


-- | Will _always_ create a new UUID for the UserId
addUserQuery :: BRApp context err => User -> DB context err User
addUserQuery user'@UserT{..} = do
  -- The id is updated inside here to that it is generated as part of the
  -- transaction so if the transaction happens to fail because the UUID
  -- generated already exists it can be rerun in entirity hopefully with a
  -- better outcome.
  userId <- newUUID
  let user = user'{user_id = userId}

  res <- -- handleError errHandler $
         pg $ runInsertReturningList (_users businessRegistryDB) $
            insertValues [user]
  case res of
        [r] -> return r
        -- TODO: Have a proper error response
        _   -> throwing _BusinessCreationError (show res)
  -- where
  --   errHandler :: (AsSqlError err, MonadError err m) => err -> m a
  --   errHandler e = case e ^? _DatabaseError of
  --     Nothing -> throwError e
  --     Just sqlErr -> case constraintViolation sqlErr of
  --       Just (UniqueViolation "users_email_address_key")
  --         -> throwing_ _UserExists
  --       _ -> throwing _InsertionFail (toServerError (Just . sqlState) sqlErr, email)
