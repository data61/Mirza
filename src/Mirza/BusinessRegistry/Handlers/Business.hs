{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Mirza.BusinessRegistry.Handlers.Business
  ( listBusinesses
  , listBusinessesQuery
  , addBusinessQuery
  ) where


import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types             as BT

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions


listBusinesses :: BRApp context err => AppM context err [BusinessResponse]
listBusinesses = fmap bizToBizResponse <$> runDb listBusinessesQuery


bizToBizResponse :: Business -> BusinessResponse
bizToBizResponse BusinessT{..} = BusinessResponse
  { bizId    = biz_gs1_company_prefix
  , bizName  = biz_name
  }


listBusinessesQuery :: BRApp context err => DB context err [Business]
listBusinessesQuery = pg $ runSelectReturningList $ select $
  all_ (_businesses businessRegistryDB)


-- | Will _always_ create a new UUID for the BizId
addBusinessQuery :: BRApp context err => Business -> DB context err Business
addBusinessQuery biz@BusinessT{..} = do
  res <- -- handleError errHandler $
         pg $ runInsertReturningList (_businesses businessRegistryDB) $
            insertValues [biz]
  case res of
        [r] -> return r
        -- TODO: Have a proper error response
        _   -> throwing _BusinessCreationErrorBRE (show res)
  -- where
  --   errHandler :: (AsSqlError err, MonadError err m) => err -> m a
  --   errHandler e = case e ^? _DatabaseError of
  --     Nothing -> throwError e
  --     Just sqlErr -> case constraintViolation sqlErr of
  --       Just (UniqueViolation "users_email_address_key")
  --         -> throwing_ _BusinessExists
  --       _ -> throwing _InsertionFail (toServerError (Just . sqlState) sqlErr, email)
