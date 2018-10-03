{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Mirza.BusinessRegistry.Handlers.Business
  ( listBusinesses
  , listBusinessesQuery
  , addBusiness
  , addBusinessAuth
  , addBusinessQuery
  ) where


import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.BusinessRegistry.SqlUtils

import           Data.GS1.EPC                             as EPC

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           Control.Lens                             ((#))

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

-- This function is an interface adapter and adds the BT.AuthUser argument to
-- addBusiness so that we can use it from behind the private API. This argument
-- is not used in the current implementation as it is assumed that all users
-- will have the ability to act globally.
addBusinessAuth ::  (BRApp context err) => BT.AuthUser -> NewBusiness -> AppM context err GS1CompanyPrefix
addBusinessAuth _ = addBusiness

addBusiness ::  (BRApp context err) => NewBusiness -> AppM context err GS1CompanyPrefix
addBusiness = (fmap biz_gs1_company_prefix)
  . (handleError (handleSqlUniqueViloation "businesses_pkey" (const $ _GS1CompanyPrefixExistsBRE # ())))
  . runDb
  . addBusinessQuery
  . newBusinessToBusiness


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
        _   -> throwing _UnexpectedErrorBRE callStack
