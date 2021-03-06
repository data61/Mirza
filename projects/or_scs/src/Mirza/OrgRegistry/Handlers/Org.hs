{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module Mirza.OrgRegistry.Handlers.Org
  ( addOrg
  , addOrgAuth
  , addOrgQuery
  , addOrgMappingAuth
  , addOrgMapping
  , searchOrgs
  , searchOrgsQuery
  , getOrgInfo
  , newOrgToOrg
  , orgToOrgResponse
  ) where


import           Mirza.Common.Time                        (toDbTimestamp)
import           Mirza.OrgRegistry.Auth
import           Mirza.OrgRegistry.Database.Schema        as Schema
import           Mirza.OrgRegistry.SqlUtils
import           Mirza.OrgRegistry.Types                  as ORT

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

import           Network.URI                              (nullURI, parseURI,
                                                           uriToString)


orgToOrgResponse :: Org -> OrgResponse
orgToOrgResponse OrgT{..} = OrgResponse
  { orgResponseGS1CompanyPrefix = org_gs1_company_prefix
  , orgResponseName             = org_name
  , orgResponseUrl              = maybe nullURI id $ parseURI $ unpack org_url
  }


newOrgToOrg :: NewOrg -> Org
newOrgToOrg NewOrg{..} =
  OrgT
    { org_gs1_company_prefix = newOrgGS1CompanyPrefix
    , org_name               = newOrgName
    , org_url                = pack $ uriToString id newOrgUrl ""
    , org_last_update        = Nothing
    }

partialNewOrgToNewOrg :: GS1CompanyPrefix -> PartialNewOrg -> NewOrg
partialNewOrgToNewOrg gs1CompanyPrefix partialNewOrg =
  NewOrg
  { newOrgGS1CompanyPrefix = gs1CompanyPrefix
  , newOrgName = partialNewOrgName partialNewOrg
  , newOrgUrl = partialNewOrgUrl partialNewOrg
  }

-- This function is an interface adapter and adds the ORT.AuthUser argument to
-- addOrg so that we can use it from behind the private API. This argument
-- is not used in the current implementation as it is assumed that all users
-- will have the ability to act globally.
addOrgAuth :: ( Member context '[HasDB]
              , Member err     '[AsORError, AsSqlError])
           => ORT.AuthUser -> GS1CompanyPrefix -> PartialNewOrg -> AppM context err NoContent
addOrgAuth authUser gs1CompanyPrefix partialNewOrg = do
  _ <- addOrg (authUserId authUser) $ partialNewOrgToNewOrg gs1CompanyPrefix partialNewOrg
  pure NoContent


addOrg :: ( Member context '[HasDB]
          , Member err     '[AsORError, AsSqlError])
       => OAuthSub -> NewOrg -> AppM context err GS1CompanyPrefix
addOrg oAuthSub = (fmap org_gs1_company_prefix)
  . (handleError (transformSqlUniqueViloation "orgs_pkey" (const $ _GS1CompanyPrefixExistsORE # ())))
  . runDb
  . addOrgAndInitialUserQuery oAuthSub
  . newOrgToOrg


addOrgAndInitialUserQuery :: (AsORError err, HasCallStack)
                          => OAuthSub -> Org -> DB context err Org
addOrgAndInitialUserQuery oAuthSub org = do
  insertedOrg  <- addOrgQuery org
  _insertedMapping <- addOrgMappingQuery (org_gs1_company_prefix insertedOrg) oAuthSub
  pure insertedOrg


-- Note: This function is separated from addOrgAndInitialUserQuery to separate concerns and inputs during design,
--       however from a use perspective you will almost definately want to call addOrgAndInitialUserQuery to make
--       sure that the company also has an initial user setup.
addOrgQuery :: (AsORError err, HasCallStack)
            => Org -> DB context err Org
addOrgQuery org@OrgT{..} = do
  result <- pg $ runInsertReturningList $ insert (_orgs orgRegistryDB)
            $ insertValues [org]
  case result of
    [insertedOrg] -> pure insertedOrg
    _             -> throwing _UnexpectedErrorORE callStack


addOrgMappingAuth :: ( Member context '[HasDB]
                     , Member err     '[AsORError, AsSqlError])
                  => ORT.AuthUser -> GS1CompanyPrefix -> OAuthSub -> AppM context err NoContent
addOrgMappingAuth authUser gs1CompanyPrefix addedUserId = do
  _ <- runDb $ userOrganisationAuthorisationQuery authUser gs1CompanyPrefix
  _ <- addOrgMapping gs1CompanyPrefix addedUserId
  pure NoContent


addOrgMapping :: ( Member context '[HasDB]
                 , Member err     '[AsORError, AsSqlError])
              => GS1CompanyPrefix -> OAuthSub -> AppM context err OrgMapping
addOrgMapping prefix oAuthSub = do
  -- We really probably should get the OrgMapping from the database here rather then constructing a new one,
  -- which will mean that the updated time is correct rather then being empty, but this is not perfectly clean either
  -- since in "theory" we need to permit for that database operation to fail. Other options include not returning the
  -- OrgMappingT at all. Constructing the OrgMappingT seems reasonable for now.
  (handleError (handleSqlUniqueViloation "org_mapping_pkey" (const $ pure (OrgMappingT (OrgPrimaryKey prefix) (Schema.UserPrimaryKey oAuthSub) Nothing))))
    $ runDb
    $ (addOrgMappingQuery prefix oAuthSub)


addOrgMappingQuery :: (AsORError err, HasCallStack)
                   => GS1CompanyPrefix -> OAuthSub -> DB context err OrgMapping
addOrgMappingQuery prefix oAuthSub = do
  checkUserExistsQuery oAuthSub

  result <- pg $ runInsertReturningList $ insert (_orgMapping orgRegistryDB)
            $ insertValues [OrgMappingT (OrgPrimaryKey prefix) (Schema.UserPrimaryKey oAuthSub) Nothing]
  case result of
    [insertedOrgMapping] -> pure insertedOrgMapping
    _                    -> throwing _UnexpectedErrorORE callStack


searchOrgs :: ( Member context '[HasDB]
              , Member err     '[AsSqlError])
           => Maybe GS1CompanyPrefix
           -> Maybe Text
           -> Maybe UTCTime
           -> AppM context err [OrgResponse]
searchOrgs mpfx mname mafter =
    fmap orgToOrgResponse <$> runDb (searchOrgsQuery mpfx mname mafter)

searchOrgsQuery :: Maybe GS1CompanyPrefix -> Maybe Text -> Maybe UTCTime -> DB context err [Org]
searchOrgsQuery mpfx mname mafter = pg $ runSelectReturningList $ select $ do
  org <- all_ (_orgs orgRegistryDB)
  for_ mpfx $ \pfx -> guard_ (org_gs1_company_prefix org ==. val_ pfx)
  for_ mname $ \name -> guard_ (org_name org `like_` val_ ("%"<>name<>"%"))
  for_ mafter $ \after -> guard_ (org_last_update org >=. just_ (val_ (toDbTimestamp after)))
  pure org


getOrgInfo :: ( Member context '[HasDB]
              , Member err     '[AsORError, AsSqlError])
           => ORT.AuthUser
           -> AppM context err [OrgResponse]
getOrgInfo (ORT.AuthUser oAuthSub) = do
  orgs <- runDb $ pg $ runSelectReturningList $ select $ do
        mapping <- all_ (_orgMapping orgRegistryDB)
        guard_ (org_mapping_user_oauth_sub mapping ==. val_ (Schema.UserPrimaryKey oAuthSub))
        pure $ org_mapping_gs1_company_prefix mapping
  let queryOrganistaion gs1CompanyPrefix = fmap orgToOrgResponse <$> runDb (searchOrgsQuery (Just gs1CompanyPrefix) Nothing Nothing)
      companyPrefixes :: [GS1CompanyPrefix]
      companyPrefixes = orgPrimaryKeyToGS1CompanyPrefix <$> orgs
  concat <$> traverse queryOrganistaion companyPrefixes
