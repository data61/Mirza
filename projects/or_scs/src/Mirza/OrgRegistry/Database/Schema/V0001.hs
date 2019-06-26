{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | This module contains all the table definitions
-- Convention: Table types and constructors are suffixed with T (for Table).
module Mirza.OrgRegistry.Database.Schema.V0001 where

import           Mirza.OrgRegistry.Types       (OAuthSub (..), oAuthSubType)

import qualified Data.GS1.EPC                  as EPC
import           Mirza.Common.Beam             (lastUpdateField)
import           Mirza.Common.GS1BeamOrphans
import           Mirza.Common.Types            (PrimaryKeyType)

import           Control.Lens

import           Crypto.JOSE.JWK               (JWK)
import           Data.Text                     (Text)
import           Data.Time                     (LocalTime)
import           Data.UUID                     (UUID)

import           Database.Beam                 as B
import           Database.Beam.Migrate.SQL     as BSQL
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres        (PgCommandSyntax, PgJSON,
                                                Postgres, json, text, uuid)
import           Database.Beam.Postgres.Syntax (PgDataTypeSyntax)

import           Data.Aeson                    hiding (json)
import           Data.Swagger

import           GHC.Generics                  (Generic)


-- Convention: Table types and constructors are suffixed with T (for Table).

--------------------------------------------------------------------------------
-- Constants and Utils
--------------------------------------------------------------------------------

defaultFieldMaxLength :: Word
defaultFieldMaxLength = 120

pkSerialType :: DataType PgDataTypeSyntax UUID
pkSerialType = uuid

-- Database
data OrgRegistryDB f = OrgRegistryDB
  { _orgs       :: f (TableEntity OrgT)
  , _users      :: f (TableEntity UserT)
  , _orgMapping :: f (TableEntity OrganisationMappingT)
  , _keys       :: f (TableEntity KeyT)
  }
  deriving Generic
instance Database anybackend OrgRegistryDB

-- Migration: Intialisation -> V1.
migration :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres OrgRegistryDB)
migration () =
  OrgRegistryDB
    <$> createTable "orgs" (OrgT
          (field "org_gs1_company_prefix" gs1CompanyPrefixType)
          (field "org_name" (varchar (Just defaultFieldMaxLength)) notNull)
          (field "org_url"  (text) notNull)
          lastUpdateField
          )
    <*> createTable "users" (UserT
          (field "oauth_sub" oAuthSubType notNull)
          lastUpdateField
          )
    <*> createTable "org_mapping" (OrganisationMappingT
          (OrgId $ field "mapping_org_id" gs1CompanyPrefixType)
          (UserPrimaryKey $ field "mapping_user_oauth_sub" oAuthSubType)
          lastUpdateField
          )
    <*> createTable "keys" (KeyT
          (field "key_id" pkSerialType)
          (UserPrimaryKey $ field "key_user_id" oAuthSubType)
          (field "jwk" json notNull)
          (field "creation_time" timestamp)
          (field "revocation_time" (maybeType timestamp))
          (UserPrimaryKey $ field "revoking_user_id" (maybeType oAuthSubType))
          (field "expiration_time" (maybeType timestamp))
          lastUpdateField
          )

--------------------------------------------------------------------------------
-- User table.
--------------------------------------------------------------------------------

type User = UserT Identity
deriving instance Show User

data UserT f = UserT
  { user_oauth_sub   :: C f OAuthSub
  , user_last_update :: C f (Maybe LocalTime)
  } deriving Generic

type UserPrimaryKey = PrimaryKey UserT Identity
deriving instance Show (PrimaryKey UserT Identity)
instance ToSchema UserPrimaryKey
instance ToParamSchema UserPrimaryKey
instance ToJSON (PrimaryKey UserT Identity) where
  toJSON (UserPrimaryKey uid) = toJSON uid
instance FromJSON (PrimaryKey UserT Identity) where
  parseJSON = fmap UserPrimaryKey . parseJSON

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserPrimaryKey (C f OAuthSub)
    deriving Generic
  primaryKey = UserPrimaryKey . user_oauth_sub
deriving instance Eq (PrimaryKey UserT Identity)


--------------------------------------------------------------------------------
-- Org Table
--------------------------------------------------------------------------------

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 11

type Org = OrgT Identity
deriving instance Show Org

data OrgT f = OrgT
  { org_gs1_company_prefix :: C f EPC.GS1CompanyPrefix
  , org_name               :: C f Text
  , org_url                :: C f Text
  , org_last_update        :: C f (Maybe LocalTime)
  }
  deriving Generic

type OrgId = PrimaryKey OrgT Identity
deriving instance Show (PrimaryKey OrgT Identity)

instance Beamable OrgT
instance Beamable (PrimaryKey OrgT)

instance Table OrgT where
  data PrimaryKey OrgT f = OrgId (C f EPC.GS1CompanyPrefix)
    deriving Generic
  primaryKey = OrgId . org_gs1_company_prefix
deriving instance Eq (PrimaryKey OrgT Identity)


--------------------------------------------------------------------------------
-- Organisation Mapping Table
--------------------------------------------------------------------------------

type OrganisationMapping = OrganisationMappingT Identity
deriving instance Show OrganisationMapping

data OrganisationMappingT f = OrganisationMappingT
  { org_mapping_gs1_company_prefix :: PrimaryKey OrgT f
  , org_mapping_user_oauth_sub     :: PrimaryKey UserT f
  , org_mapping_last_update        :: C f (Maybe LocalTime)
  }
  deriving Generic

type OrganisationMappingId = PrimaryKey OrganisationMappingT Identity
deriving instance Show (PrimaryKey OrganisationMappingT Identity)

instance Beamable OrganisationMappingT
instance Beamable (PrimaryKey OrganisationMappingT)

instance Table OrganisationMappingT where
  data PrimaryKey OrganisationMappingT f = OrganisationMappingId (PrimaryKey OrgT f) (PrimaryKey UserT f)
    deriving Generic
  primaryKey = OrganisationMappingId <$> org_mapping_gs1_company_prefix <*> org_mapping_user_oauth_sub
deriving instance Eq (PrimaryKey OrganisationMappingT Identity)


--------------------------------------------------------------------------------
-- Keys Table
--------------------------------------------------------------------------------

type Key = KeyT Identity
deriving instance Show Key
deriving instance Show ( PrimaryKey UserT (Nullable Identity))

-- The types are not ``UTCTime`` because beam does not support UTCTime
-- See this discussion for details:
-- https://groups.google.com/forum/#!topic/beam-discussion/DcC0yik7Pxc
-- However, all times are converted to UTCTime using methods from typeclasses
-- defined in Mirza.Common.Time
data KeyT f = KeyT
  { key_id           :: C f PrimaryKeyType
  , key_user_id      :: PrimaryKey UserT f    -- TODO: We should record the org that is associated with the key...not sure if there is any need to store the user...
  , key_jwk          :: C f (PgJSON JWK)
  , creation_time    :: C f LocalTime -- Stored as UTC Time
  -- It would be nicer and cleaner to store the revocation time and user as a
  -- Maybe (LocalTime, UserId) rather then as two independent Maybe fields as
  -- they should only ever be stored in composite and would prevent accidental
  -- errors where they are not stored the same (i.e. one is a Just and the other
  -- is Nothing), but currently we don't know how to do this / if it is even
  -- possible and so have this implementation for now...
  , revocation_time  :: C f (Maybe LocalTime) -- Stored as UTC Time
  , revoking_user_id :: PrimaryKey UserT (Nullable f)
  , expiration_time  :: C f (Maybe LocalTime) -- Stored as UTC Time
  , key_last_update  :: C f (Maybe LocalTime)
  }
  deriving Generic

type KeyId = PrimaryKey KeyT Identity
deriving instance Show (PrimaryKey KeyT Identity)

instance Beamable KeyT
instance Beamable (PrimaryKey KeyT)

instance Table KeyT where
  data PrimaryKey KeyT f = KeyId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = KeyId . key_id
deriving instance Eq (PrimaryKey KeyT Identity)
