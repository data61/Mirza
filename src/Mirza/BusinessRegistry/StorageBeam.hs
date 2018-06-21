{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- | This module contains all the table definitions
-- The migration script has been moved to the module MigrateScript
-- If some definition is changed here, please make the equivalent change
-- in MigrateScript
module Mirza.BusinessRegistry.StorageBeam where

import qualified Data.GS1.EPC           as EPC
import qualified Data.GS1.Event         as Ev

import           Control.Lens
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.ByteString        (ByteString)
import           Data.Swagger           (ToSchema)
import           Data.Text              (Text)

import           Data.Time              (LocalTime)
import           Data.UUID              (UUID)

import           Database.Beam          as B
import           Database.Beam.Postgres



data BusinessRegistryDB f = BusinessRegistryDB
  { _users      :: f (TableEntity UserT)
  , _keys       :: f (TableEntity KeyT)
  , _businesses :: f (TableEntity BusinessT)
  }
  deriving Generic
instance Database be BusinessRegistryDB

-- | Everything that comes after ``withDbModification`` is primarily
-- foreign keys that have been forced to retain their own names
supplyChainDb :: DatabaseSettings Postgres BusinessRegistryDB
supplyChainDb = defaultDbSettings
  `withDbModification`
  dbModification
    {
      _users =
        modifyTable (const "users") $
        tableModification
        {
          user_biz_id = BizId (fieldNamed "user_biz_id")
        }
    , _keys =
        modifyTable (const "keys") $
        tableModification
        {
          key_user_id = UserId (fieldNamed "key_user_id")
        }
    , _businesses =
        modifyTable (const "businesses") $
        tableModification
        -- {
        --   biz_gs1_company_prefix = BizId (fieldNamed "biz_company_prefix")
        -- }
    }



type PrimaryKeyType = UUID

data UserT f = User
  { user_id       :: C f PrimaryKeyType
  , user_biz_id   :: PrimaryKey BusinessT f
  , first_name    :: C f Text
  , last_name     :: C f Text
  , phone_number  :: C f Text
  , password_hash :: C f ByteString --XXX - should this be blob?
  , email_address :: C f Text }
  deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Show (PrimaryKey UserT Identity)

instance Beamable UserT
instance Beamable (PrimaryKey UserT)

instance Table UserT where
  data PrimaryKey UserT f = UserId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = UserId . user_id
deriving instance Eq (PrimaryKey UserT Identity)

data KeyT f = Key
  { key_id          :: C f PrimaryKeyType
  , key_user_id     :: PrimaryKey UserT f
  , pem_str         :: C f Text
  , creation_time   :: C f LocalTime -- UTCTime
  , revocation_time :: C f (Maybe LocalTime) -- UTCTime
  , expiration_time :: C f (Maybe LocalTime) -- UTCTime
  }
  deriving Generic
type Key = KeyT Identity
type KeyId = PrimaryKey KeyT Identity

deriving instance Show Key

instance Beamable KeyT
instance Beamable (PrimaryKey KeyT)

instance Table KeyT where
  data PrimaryKey KeyT f = KeyId (C f PrimaryKeyType)
    deriving Generic
  primaryKey = KeyId . key_id
deriving instance Eq (PrimaryKey KeyT Identity)

-- CBV-Standard-1-2-r-2016-09-29.pdf Page 11

data BusinessT f = Business
  { biz_gs1_company_prefix :: C f EPC.GS1CompanyPrefix -- PrimaryKey
  , biz_name               :: C f Text
  , biz_function           :: C f Text
  , biz_site_name          :: C f Text
  , biz_address            :: C f Text
  , biz_lat                :: C f Double
  , biz_long               :: C f Double }
  deriving Generic
type Business = BusinessT Identity
type BizId = PrimaryKey BusinessT Identity

deriving instance Show Business

instance Beamable BusinessT
instance Beamable (PrimaryKey BusinessT)
deriving instance Show (PrimaryKey BusinessT Identity)

instance Table BusinessT where
  data PrimaryKey BusinessT f = BizId (C f EPC.GS1CompanyPrefix)
    deriving Generic
  primaryKey = BizId . biz_gs1_company_prefix
deriving instance Eq (PrimaryKey BusinessT Identity)
