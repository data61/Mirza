{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Mirza.BusinessRegistry.Handlers.Business
  ( addBusiness
  , addBusinessAuth
  , addBusinessQuery
  , searchBusinesses
  , searchBusinessesQuery
  ) where


import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.SqlUtils
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Time                        (toDbTimestamp)

import           Data.GS1.EPC                             as EPC

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           Control.Lens                             (( # ))

import           Data.Foldable                            (for_)
import           Data.Text                                (Text)
import           Data.Time                                (UTCTime)
import           GHC.Stack                                (HasCallStack,
                                                           callStack)


businessToBusinessResponse :: Business -> BusinessResponse
businessToBusinessResponse BusinessT{..} = BusinessResponse
  { businessGS1CompanyPrefix = biz_gs1_company_prefix
  , businessName             = biz_name
  }


-- This function is an interface adapter and adds the BT.AuthUser argument to
-- addBusiness so that we can use it from behind the private API. This argument
-- is not used in the current implementation as it is assumed that all users
-- will have the ability to act globally.
addBusinessAuth :: ( Member context '[HasDB]
                   , Member err     '[AsBRError, AsSqlError])
                => BT.AuthUser -> NewBusiness -> AppM context err GS1CompanyPrefix
addBusinessAuth _ = addBusiness

addBusiness :: ( Member context '[HasDB]
               , Member err     '[AsBRError, AsSqlError])
            => NewBusiness -> AppM context err GS1CompanyPrefix
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
    , biz_last_update        = Nothing
    }


addBusinessQuery :: (AsBRError err, HasCallStack)
                 => Business -> DB context err Business
addBusinessQuery biz@BusinessT{..} = do
  res <- pg $ runInsertReturningList (_businesses businessRegistryDB)
            $ insertValues [biz]
  case res of
        [r] -> pure r
        _   -> throwing _UnexpectedErrorBRE callStack


searchBusinesses :: ( Member context '[HasDB]
                  , Member err     '[AsSqlError])
               => Maybe GS1CompanyPrefix -> Maybe Text -> Maybe UTCTime -> AppM context err [BusinessResponse]
searchBusinesses mpfx mname mafter = fmap businessToBusinessResponse <$> runDb (searchBusinessesQuery mpfx mname mafter)

searchBusinessesQuery :: Maybe GS1CompanyPrefix -> Maybe Text -> Maybe UTCTime -> DB context err [Business]
searchBusinessesQuery mpfx mname mafter = pg $ runSelectReturningList $ select $ do
  biz <- all_ (_businesses businessRegistryDB)
  for_ mpfx $ \pfx -> guard_ (biz_gs1_company_prefix biz ==. val_ pfx)
  for_ mname $ \name -> guard_ (biz_name biz `like_` val_ ("%"<>name<>"%"))
  for_ mafter $ \after -> guard_ (biz_last_update biz >=. just_ (val_ (toDbTimestamp after)))
  pure biz
