{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Contains the migration function of ``businessRegistryDb``
module Mirza.BusinessRegistry.MigrateScript (migrationStorage) where

import           Mirza.BusinessRegistry.StorageBeam

import qualified Data.GS1.EPC                         as EPC

import qualified Database.Beam                        as B
import qualified Database.Beam.Backend.SQL            as BSQL
import qualified Database.Beam.Migrate                as BMigrate
import           Database.Beam.Migrate.SQL            (DataType)
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres
import qualified Database.Beam.Postgres               as BPostgres
import           Database.Beam.Postgres.Syntax        (PgDataTypeSyntax,
                                                       pgTextType)
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.ToField   (ToField (..))

import           Data.ByteString                      (ByteString)
import qualified Data.Text                            as T
import           Data.UUID                            (UUID)
import           Text.Read                            (readMaybe)




maxLen :: Word
maxLen = 120

-- length of the timezone offset
maxTzLen :: Word
maxTzLen = 10

pkSerialType :: DataType PgDataTypeSyntax UUID
pkSerialType = uuid

migrationStorage :: () -> Migration PgCommandSyntax (CheckedDatabaseSettings Postgres BusinessRegistryDB)
migrationStorage () =
  BusinessRegistryDB
    <$> createTable "users"
    (
      User
          (field "user_id" pkSerialType)
          (BizId (field "user_biz_id" gs1CompanyPrefixType))
          (field "first_name" (varchar (Just maxLen)) notNull)
          (field "last_name" (varchar (Just maxLen)) notNull)
          (field "phone_number" (varchar (Just maxLen)) notNull)
          (field "password_hash" binaryLargeObject notNull)
          (field "email_address" (varchar (Just maxLen)) unique)
    )
    <*> createTable "keys"
    (
      Key
          (field "key_id" pkSerialType)
          (UserId (field "key_user_id" pkSerialType))
          (field "pem_str" text)
          (field "creation_time" timestamptz)
          (field "revocation_time" (maybeType timestamptz))
          (field "expiration_time" (maybeType timestamptz))
    )
    <*> createTable "businesses"
      (
        Business
            (field "biz_gs1_company_prefix" gs1CompanyPrefixType) -- note is primary key
            (field "biz_name" (varchar (Just maxLen)) notNull)
            (field "biz_function" (varchar (Just maxLen)) notNull)
            (field "biz_site_name" (varchar (Just maxLen)) notNull)
            (field "biz_address" (varchar (Just maxLen)) notNull)
            (field "biz_lat" double)
            (field "biz_long" double)
      )

-- TODO: Move this to common module for both servers

-- | The generic implementation of fromField
-- If it's a fromField used for ``SomeCustomType``, sample usage would be
-- instance FromField SomeCustomType where
--   fromField = defaultFromField "SomeCustomType"
defaultFromField :: (B.Typeable b, Read b) => String
                 -> Field
                 -> Maybe ByteString
                 -> Conversion b
defaultFromField fName f bs = do
  x <- readMaybe <$> fromField f bs
  case x of
    Nothing ->
      returnError ConversionFailed
        f $ "Could not 'read' value for " ++ fName
    Just val -> pure val


-- | Shorthand for using postgres text type
textType :: BMigrate.DataType PgDataTypeSyntax a
textType = BMigrate.DataType pgTextType


-- | Helper function to manage the returnValue of ``readMaybe`` or gracefully
-- fail
handleReadColumn :: Monad f => Maybe a -> String -> String -> f a
handleReadColumn (Just a) _ _= pure a
handleReadColumn Nothing colName val =
    fail ("Invalid value for " ++ colName ++ ": " ++ val)

-- defaultFromBackendRow :: String
--                       -> F (FromBackendRowF be) b
-- | Wrapper that calls read and fail appropriately
-- An explicit definition of ``fromBackendRow`` is required for each custom type
defaultFromBackendRow :: (Read a, BSQL.FromBackendRow be T.Text)
                      => String -> BSQL.FromBackendRowM be a
defaultFromBackendRow colName = do
  val <- BSQL.fromBackendRow
  let valStr = T.unpack val
  handleReadColumn (readMaybe valStr) colName valStr


-- ======= EPC.GS1CompanyPrefix =======

instance (BSQL.HasSqlValueSyntax be String)
        => BSQL.HasSqlValueSyntax be EPC.GS1CompanyPrefix where
  sqlValueSyntax = BSQL.autoSqlValueSyntax

instance (BMigrate.IsSql92ColumnSchemaSyntax be) =>
  BMigrate.HasDefaultSqlDataTypeConstraints be EPC.GS1CompanyPrefix

instance  (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool
          , BSQL.IsSql92ExpressionSyntax be)
          => B.HasSqlEqualityCheck be EPC.GS1CompanyPrefix


instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool
        , (BSQL.IsSql92ExpressionSyntax be))
  => B.HasSqlQuantifiedEqualityCheck be EPC.GS1CompanyPrefix

instance BSQL.FromBackendRow BPostgres.Postgres EPC.GS1CompanyPrefix where
  fromBackendRow = defaultFromBackendRow "EPC.GS1CompanyPrefix"

instance FromField EPC.GS1CompanyPrefix where
  fromField = defaultFromField "EPC.GS1CompanyPrefix"

instance ToField EPC.GS1CompanyPrefix where
  toField = toField . show

gs1CompanyPrefixType :: BMigrate.DataType PgDataTypeSyntax EPC.GS1CompanyPrefix
gs1CompanyPrefixType = textType
