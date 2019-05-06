{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Mirza.BusinessRegistry.Handlers.Business
  ( addBusiness
  , addBusinessAuth
  , addBusinessQuery
  , addOrganisationMappingAuth
  , addOrganisationMapping
  , searchBusinesses
  , searchBusinessesQuery
  , getBusinessInfo
  , newBusinessToBusiness
  , businessToBusinessResponse
  ) where


import           Mirza.BusinessRegistry.Auth
import           Mirza.BusinessRegistry.Database.Schema   as Schema
import           Mirza.BusinessRegistry.SqlUtils
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Time                        (toDbTimestamp)

import           Data.GS1.EPC                             as EPC

import           Servant.API                              (NoContent (..))

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           Control.Lens                             (( # ))

import           Data.Foldable                            (for_)
import           Data.Text                                (Text, pack, unpack)
import           Data.Time                                (UTCTime)
import           GHC.Stack                                (HasCallStack,
                                                           callStack)

import           Network.URI                              (uriToString, parseURI, nullURI)


businessToBusinessResponse :: Business -> BusinessResponse
businessToBusinessResponse BusinessT{..} = BusinessResponse
  { businessGS1CompanyPrefix = business_gs1_company_prefix
  , businessName             = business_name
  , businessUrl              = maybe nullURI id $ parseURI $ unpack business_url
  }


newBusinessToBusiness :: NewBusiness -> Business
newBusinessToBusiness NewBusiness{..} =
  BusinessT
    { business_gs1_company_prefix = newBusinessGS1CompanyPrefix
    , business_name               = newBusinessName
    , business_url                = pack $ uriToString id newBusinessUrl ""
    , business_last_update        = Nothing
    }


-- This function is an interface adapter and adds the BT.AuthUser argument to
-- addBusiness so that we can use it from behind the private API. This argument
-- is not used in the current implementation as it is assumed that all users
-- will have the ability to act globally.
addBusinessAuth :: ( Member context '[HasDB]
                   , Member err     '[AsBRError, AsSqlError])
                => BT.AuthUser -> NewBusiness -> AppM context err GS1CompanyPrefix
addBusinessAuth authUser = addBusiness (authUserId authUser)

addBusiness :: ( Member context '[HasDB]
               , Member err     '[AsBRError, AsSqlError])
            => BT.UserId -> NewBusiness -> AppM context err GS1CompanyPrefix
addBusiness userId = (fmap business_gs1_company_prefix)
  . (handleError (handleSqlUniqueViloation "businesses_pkey" (const $ _GS1CompanyPrefixExistsBRE # ())))
  . runDb
  . addBusinessAndInitialUserQuery userId
  . newBusinessToBusiness


addBusinessAndInitialUserQuery :: (AsBRError err, HasCallStack)
                               => BT.UserId -> Business -> DB context err Business
addBusinessAndInitialUserQuery user business = do
  insertedBusiness  <- addBusinessQuery business
  _insertedMapping <- addOrganisationMappingQuery (business_gs1_company_prefix insertedBusiness) user
  pure insertedBusiness


-- Note: This function is separated from addBusinessAndInitialUserQuery to separate concerns and inputs during design,
--       however from a use perspective you will almost definately want to call addBusinessAndInitialUserQuery to make
--       sure that the company also has an initial user setup.
addBusinessQuery :: (AsBRError err, HasCallStack)
                 => Business -> DB context err Business
addBusinessQuery biz@BusinessT{..} = do
  result <- pg $ runInsertReturningList (_businesses businessRegistryDB)
            $ insertValues [biz]
  case result of
    [insertedBusiness] -> pure insertedBusiness
    _                  -> throwing _UnexpectedErrorBRE callStack

addOrganisationMappingAuth :: ( Member context '[HasDB]
                              , Member err     '[AsBRError, AsSqlError])
                           => BT.AuthUser -> GS1CompanyPrefix -> BT.UserId -> AppM context err NoContent
addOrganisationMappingAuth authUser gs1CompanyPrefix addedUserId = do
  runDb $ userOrganisationAutherisation authUser gs1CompanyPrefix
  addOrganisationMapping gs1CompanyPrefix addedUserId
  pure NoContent


addOrganisationMapping :: ( Member context '[HasDB]
                          , Member err     '[AsBRError, AsSqlError])
                       => GS1CompanyPrefix -> BT.UserId -> AppM context err OrganisationMapping
addOrganisationMapping prefix user = runDb $ addOrganisationMappingQuery prefix user


addOrganisationMappingQuery :: (AsBRError err, HasCallStack)
                            => GS1CompanyPrefix -> BT.UserId -> DB context err OrganisationMapping
addOrganisationMappingQuery prefix userId = do
  checkUserExistsQuery userId

  result <- pg $ runInsertReturningList (_organisationMapping businessRegistryDB)
            $ insertValues [OrganisationMappingT (BizId prefix) (Schema.UserId $ getUserId userId) Nothing]
  case result of
    [insertedOrganisationMapping] -> pure insertedOrganisationMapping
    _                             -> throwing _UnexpectedErrorBRE callStack


searchBusinesses :: ( Member context '[HasDB]
                    , Member err     '[AsSqlError])
                 => Maybe GS1CompanyPrefix
                 -> Maybe Text
                 -> Maybe UTCTime
                 -> AppM context err [BusinessResponse]
searchBusinesses mpfx mname mafter =
    fmap businessToBusinessResponse <$> runDb (searchBusinessesQuery mpfx mname mafter)

searchBusinessesQuery :: Maybe GS1CompanyPrefix -> Maybe Text -> Maybe UTCTime -> DB context err [Business]
searchBusinessesQuery mpfx mname mafter = pg $ runSelectReturningList $ select $ do
  biz <- all_ (_businesses businessRegistryDB)
  for_ mpfx $ \pfx -> guard_ (business_gs1_company_prefix biz ==. val_ pfx)
  for_ mname $ \name -> guard_ (business_name biz `like_` val_ ("%"<>name<>"%"))
  for_ mafter $ \after -> guard_ (business_last_update biz >=. just_ (val_ (toDbTimestamp after)))
  pure biz


getBusinessInfo :: ( Member context '[HasDB]
                   , Member err     '[AsBRError, AsSqlError])
                => BT.AuthUser
                -> AppM context err [BusinessResponse]
getBusinessInfo (BT.AuthUser (BT.UserId uId)) = do
  organisations <- runDb $ pg $ runSelectReturningList $ select $ do
        mapping <- all_ (_organisationMapping businessRegistryDB)
        guard_ (organisation_mapping_user_id mapping ==. val_ (Schema.UserId uId))
        pure $ organisation_mapping_gs1_company_prefix mapping
  let queryOrganistaion gs1CompanyPrefix = fmap businessToBusinessResponse <$> runDb (searchBusinessesQuery (Just gs1CompanyPrefix) Nothing Nothing)
      getPrefix :: PrimaryKey BusinessT Identity -> GS1CompanyPrefix
      getPrefix (BizId prefix) = prefix
      companyPrefixes :: [GS1CompanyPrefix]
      companyPrefixes = getPrefix <$> organisations
  concat <$> traverse queryOrganistaion companyPrefixes
