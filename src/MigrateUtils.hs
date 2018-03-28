{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- | This module contains the
-- Database.Beam.BPostgres.Postgres.Syntax.BMigrate.DataType definitions
-- At the moment, if Database.Beam.BPostgres.Postgres.Syntax is a hidden module
-- So it is not possible to implement the types yet
module MigrateUtils where

import qualified Database.Beam                        as B
import qualified Database.Beam.Backend.SQL            as BSQL
import qualified Database.Beam.Migrate                as BMigrate
import qualified Database.Beam.Postgres               as BPostgres

import           Data.ByteString                      (ByteString)
import qualified Data.Text                            as T
import           Database.PostgreSQL.Simple.FromField
import           Text.Read

import qualified Data.GS1.EPC                         as EPC
import qualified Data.GS1.Event                       as Ev
import           Database.Beam.Postgres.Syntax        (PgDataTypeSyntax,
                                                       pgTextType)
import           Database.PostgreSQL.Simple.ToField   (ToField, toField)
import           GHC.Generics                         (Generic)

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

handleReadColumn :: Monad f => Maybe a -> String -> String -> f a
handleReadColumn (Just a) _ _= pure a
handleReadColumn Nothing colName val =
    fail ("Invalid value for " ++ colName ++ ": " ++ val)

-- defaultFromBackendRow :: String
--                       -> F (FromBackendRowF be) b
-- | Wrapper that calls read and fail appropriately
-- An explicit definition of ``fromBackendRow`` is required for each custom type
defaultFromBackendRow colName = do
  val <- BSQL.fromBackendRow
  let valStr = T.unpack val
  handleReadColumn (readMaybe valStr) colName valStr

-- Type definitions

-- ======= Event Type =======

instance BSQL.HasSqlValueSyntax be String =>
  BSQL.HasSqlValueSyntax be Ev.EventType where
    sqlValueSyntax = BSQL.autoSqlValueSyntax
instance (BMigrate.IsSql92ColumnSchemaSyntax be) =>
  BMigrate.HasDefaultSqlDataTypeConstraints be Ev.EventType

instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlEqualityCheck be Ev.EventType
instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlQuantifiedEqualityCheck be Ev.EventType

instance BSQL.FromBackendRow BPostgres.Postgres Ev.EventType where
  fromBackendRow = defaultFromBackendRow "Ev.EventType"

instance FromField Ev.EventType where
  fromField = defaultFromField "Ev.EventType"

instance ToField Ev.EventType where
  toField = toField . show

eventType :: BMigrate.DataType PgDataTypeSyntax Ev.EventType
eventType = textType


-- ======= EPC.LocationReference =======
instance BSQL.HasSqlValueSyntax be String =>
  BSQL.HasSqlValueSyntax be EPC.LocationReference where
    sqlValueSyntax = BSQL.autoSqlValueSyntax
instance (BMigrate.IsSql92ColumnSchemaSyntax be) =>
  BMigrate.HasDefaultSqlDataTypeConstraints be EPC.LocationReference

instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlEqualityCheck be EPC.LocationReference
instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlQuantifiedEqualityCheck be EPC.LocationReference

instance BSQL.FromBackendRow BPostgres.Postgres EPC.LocationReference where
  fromBackendRow = defaultFromBackendRow "EPC.LocationReference"

instance FromField EPC.LocationReference where
  fromField = defaultFromField "EPC.LocationReference"

instance ToField EPC.LocationReference where
  toField = toField . show

locationRefType :: BMigrate.DataType PgDataTypeSyntax EPC.LocationReference
locationRefType = textType

-- ======= EPC.SourceDestType =======

instance BSQL.HasSqlValueSyntax be String =>
  BSQL.HasSqlValueSyntax be EPC.SourceDestType where
    sqlValueSyntax = BSQL.autoSqlValueSyntax
instance (BMigrate.IsSql92ColumnSchemaSyntax be) =>
  BMigrate.HasDefaultSqlDataTypeConstraints be EPC.SourceDestType

instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlEqualityCheck be EPC.SourceDestType
instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlQuantifiedEqualityCheck be EPC.SourceDestType

instance BSQL.FromBackendRow BPostgres.Postgres EPC.SourceDestType where
  fromBackendRow = defaultFromBackendRow "EPC.SourceDestType"

instance FromField EPC.SourceDestType where
  fromField = defaultFromField "EPC.SourceDestType"

instance ToField EPC.SourceDestType where
  toField = toField . show

srcDestType :: BMigrate.DataType PgDataTypeSyntax EPC.SourceDestType
srcDestType = textType

-- ======= EPC.Action =======

instance BSQL.HasSqlValueSyntax be String =>
  BSQL.HasSqlValueSyntax be EPC.Action where
    sqlValueSyntax = BSQL.autoSqlValueSyntax
instance (BMigrate.IsSql92ColumnSchemaSyntax be) =>
  BMigrate.HasDefaultSqlDataTypeConstraints be EPC.Action

instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlEqualityCheck be EPC.Action
instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlQuantifiedEqualityCheck be EPC.Action

instance BSQL.FromBackendRow BPostgres.Postgres EPC.Action where
  fromBackendRow = defaultFromBackendRow "EPC.Action"

instance FromField EPC.Action where
  fromField = defaultFromField "EPC.Action"

instance ToField EPC.Action where
  toField = toField . show

actionType :: BMigrate.DataType PgDataTypeSyntax EPC.Action
actionType = textType

-- ======= EPC.SGTINFilterValue ========

instance BSQL.HasSqlValueSyntax be String =>
  BSQL.HasSqlValueSyntax be EPC.SGTINFilterValue where
    sqlValueSyntax = BSQL.autoSqlValueSyntax
instance (BMigrate.IsSql92ColumnSchemaSyntax be) =>
  BMigrate.HasDefaultSqlDataTypeConstraints be EPC.SGTINFilterValue

instance FromField EPC.SGTINFilterValue where
  fromField = defaultFromField "SGTINFilterValue"

instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlEqualityCheck be EPC.SGTINFilterValue
instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlQuantifiedEqualityCheck be EPC.SGTINFilterValue

instance BSQL.FromBackendRow BPostgres.Postgres EPC.SGTINFilterValue where
  fromBackendRow = defaultFromBackendRow "EPC.SGTINFilterValue"

sgtinFilterValue :: BMigrate.DataType PgDataTypeSyntax EPC.SGTINFilterValue
sgtinFilterValue = textType

-- ======= LocationField ========

-- | The record fields in Data.GS1.DWhere for the data type DWhere
data LocationField =
    Src
  | Dest
  | BizLocation
  | ReadPoint deriving (Generic, Show, Eq, Read)

instance BSQL.HasSqlValueSyntax be String =>
  BSQL.HasSqlValueSyntax be LocationField where
    sqlValueSyntax = BSQL.autoSqlValueSyntax
instance (BMigrate.IsSql92ColumnSchemaSyntax be) =>
  BMigrate.HasDefaultSqlDataTypeConstraints be LocationField

instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlEqualityCheck be LocationField
instance (BSQL.HasSqlValueSyntax (BSQL.Sql92ExpressionValueSyntax be) Bool,
          BSQL.IsSql92ExpressionSyntax be) =>
          B.HasSqlQuantifiedEqualityCheck be LocationField

instance BSQL.FromBackendRow BPostgres.Postgres LocationField where
  fromBackendRow = defaultFromBackendRow "LocationField"

instance FromField LocationField where
  fromField = defaultFromField "LocationField"

instance ToField LocationField where
  toField = toField . show

locationType :: BMigrate.DataType PgDataTypeSyntax LocationField
locationType = textType
