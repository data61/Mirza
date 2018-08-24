{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Mirza.BusinessRegistry.Handlers.Business
  ( listBusinesses
  , listBusinessesQuery
  , addBusiness
  , addBusinessQuery
  ) where


import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types             as BT

import           Data.GS1.EPC                             as EPC

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (UniqueViolation),
                                                           constraintViolation)

import           Control.Lens                             ((^?))
import           Control.Monad.Except                     (catchError, throwError)

import           GHC.Stack                                (HasCallStack, callStack)


listBusinesses :: BRApp context err => AppM context err [BusinessResponse]
listBusinesses = fmap businessToBusinessResponse <$> runDb listBusinessesQuery


businessToBusinessResponse :: Business -> BusinessResponse
businessToBusinessResponse BusinessT{..} = BusinessResponse
  { businessGS1CompanyPrefix = biz_gs1_company_prefix
  , businessName             = biz_name
  }


listBusinessesQuery :: BRApp context err => DB context err [Business]
listBusinessesQuery = pg $ runSelectReturningList $ select $
  all_ (_businesses businessRegistryDB)


addBusiness ::  (BRApp context err) => NewBusiness -> AppM context err GS1CompanyPrefix
addBusiness = (fmap biz_gs1_company_prefix)
  . (flip catchError errHandler)
  . runDb
  . addBusinessQuery
  . newBusinessToBusiness
  where
    errHandler :: (AsSqlError err, AsBusinessRegistryError err, MonadError err m, MonadIO m) => err -> m a
    errHandler e = case e ^? _SqlError of
      Nothing -> throwError e
      Just sqlErr ->
        case constraintViolation sqlErr of
          Just (UniqueViolation "businesses_pkey") -> throwing_ _BusinessCreationErrorNonUniqueBRE
          _ -> throwError e


newBusinessToBusiness :: NewBusiness -> Business
newBusinessToBusiness NewBusiness{..} =
  BusinessT
    { biz_gs1_company_prefix = newBusinessGS1CompanyPrefix
    , biz_name               = newBusinessName
    }


addBusinessQuery :: (HasCallStack, BRApp context err) => Business -> DB context err Business
addBusinessQuery biz@BusinessT{..} = do
  res <- pg $ runInsertReturningList (_businesses businessRegistryDB)
            $ insertValues [biz]
  case res of
        [r] -> return r
        _   -> throwing _LogicErrorBRE callStack
