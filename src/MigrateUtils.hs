{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}

-- | This module contains the
-- Database.Beam.Postgres.Syntax.DataType definitions
-- At the moment, if Database.Beam.Postgres.Syntax is a hidden module
-- So it is not possible to implement the types yet
module MigrateUtils where

import           Database.Beam
import           Database.Beam.Postgres
import           Database.Beam.Migrate
import           Database.Beam.Postgres.Migrate
import           Database.Beam.Backend.SQL

import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Database.PostgreSQL.Simple.FromField
import           Text.Read

import           Database.Beam.Postgres.Syntax (PgDataTypeSyntax, pgTextType)
import qualified Data.GS1.Event as Ev
import qualified Data.GS1.EPC as EPC
import           Database.PostgreSQL.Simple.ToField (ToField, toField)


-- Utils

-- | The generic implementation of fromField
-- If it's a fromField used for ``SomeCustomType``, sample usage would be
-- instance FromField SomeCustomType where
--   fromField = defaultFromField "SomeCustomType"
defaultFromField :: (Typeable b, Read b) => String
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
textType :: DataType PgDataTypeSyntax a
textType = DataType pgTextType


-- Type definitions

-- ======= Event Type =======

instance HasSqlValueSyntax be String => HasSqlValueSyntax be Ev.EventType where
  sqlValueSyntax = autoSqlValueSyntax
instance (IsSql92ColumnSchemaSyntax be) => HasDefaultSqlDataTypeConstraints be Ev.EventType

instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlEqualityCheck be Ev.EventType
instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlQuantifiedEqualityCheck be Ev.EventType

-- | An explicit definition of ``fromBackendRow`` is required for each custom type
instance FromBackendRow Postgres Ev.EventType where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: T.Text of
      "ObjectEventT" -> pure Ev.ObjectEventT
      "AggregationEventT" -> pure Ev.AggregationEventT
      "TransactionEventT" -> pure Ev.TransactionEventT
      "TransformationEventT" -> pure Ev.TransformationEventT
      _ -> fail ("Invalid value for Ev.EventType: " ++ T.unpack val)

instance FromField Ev.EventType where
  fromField = defaultFromField "Ev.EventType"

instance ToField Ev.EventType where
  toField = toField . show

eventType :: DataType PgDataTypeSyntax Ev.EventType
eventType = textType

-- ======= EPC.SourceDestType =======

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EPC.SourceDestType where
  sqlValueSyntax = autoSqlValueSyntax
instance (IsSql92ColumnSchemaSyntax be) => HasDefaultSqlDataTypeConstraints be EPC.SourceDestType

instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlEqualityCheck be EPC.SourceDestType
instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlQuantifiedEqualityCheck be EPC.SourceDestType

-- | An explicit definition of ``fromBackendRow`` is required for each custom type
instance FromBackendRow Postgres EPC.SourceDestType where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: T.Text of
      "SDOwningParty" -> pure EPC.SDOwningParty
      "SDPossessingParty" -> pure EPC.SDPossessingParty
      "SDLocation" -> pure EPC.SDLocation
      _ -> fail ("Invalid value for EPC.SourceDestType: " ++ T.unpack val)

instance FromField EPC.SourceDestType where
  fromField = defaultFromField "EPC.SourceDestType"

instance ToField EPC.SourceDestType where
  toField = toField . show

srcDestType :: DataType PgDataTypeSyntax EPC.SourceDestType
srcDestType = textType

-- ======= EPC.Action =======

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EPC.Action where
  sqlValueSyntax = autoSqlValueSyntax
instance (IsSql92ColumnSchemaSyntax be) => HasDefaultSqlDataTypeConstraints be EPC.Action

instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlEqualityCheck be EPC.Action
instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlQuantifiedEqualityCheck be EPC.Action

-- | An explicit definition of ``fromBackendRow`` is required for each custom type
instance FromBackendRow Postgres EPC.Action where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: T.Text of
      "Add" -> pure EPC.Add
      "Observe" -> pure EPC.Observe
      "Delete" -> pure EPC.Delete
      _ -> fail ("Invalid value for EPC.Action: " ++ T.unpack val)

instance FromField EPC.Action where
  fromField = defaultFromField "EPC.Action"

instance ToField EPC.Action where
  toField = toField . show

actionType :: DataType PgDataTypeSyntax EPC.Action
actionType = textType

-- ======= EPC.SGTINFilterValue ========

instance HasSqlValueSyntax be String => HasSqlValueSyntax be EPC.SGTINFilterValue where
  sqlValueSyntax = autoSqlValueSyntax
instance (IsSql92ColumnSchemaSyntax be) => HasDefaultSqlDataTypeConstraints be EPC.SGTINFilterValue

instance FromField EPC.SGTINFilterValue where
  fromField = defaultFromField "SGTINFilterValue"

instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlEqualityCheck be EPC.SGTINFilterValue
instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlQuantifiedEqualityCheck be EPC.SGTINFilterValue

-- | An explicit definition of ``fromBackendRow`` is required for each custom type
instance FromBackendRow Postgres EPC.SGTINFilterValue where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: T.Text of
      "AllOthers" -> pure EPC.AllOthers
      "POSTradeItem" -> pure EPC.POSTradeItem
      "FullCaseForTransport" -> pure EPC.FullCaseForTransport
      "Reserved1" -> pure EPC.Reserved1
      "InnerPackTradeItemGroupingForHandling" ->
          pure EPC.InnerPackTradeItemGroupingForHandling
      "Reserved2" -> pure EPC.Reserved2
      "UnitLoad" -> pure EPC.UnitLoad
      "UnitInsideTradeItemOrComponentInsideAProductNotIntendedForIndividualSale" ->
          pure EPC.UnitInsideTradeItemOrComponentInsideAProductNotIntendedForIndividualSale
      _ -> fail ("Invalid value for EPC.SGTINFilterValue: " ++ T.unpack val)

sgtinFilterValue :: DataType PgDataTypeSyntax EPC.SGTINFilterValue
sgtinFilterValue = textType

-- ======= LocationField ========

-- | The record fields in Data.GS1.DWhere for the data type DWhere
data LocationField =
    Src
  | Dest
  | BizLocation
  | ReadPoint deriving (Generic, Show, Eq, Read)

instance HasSqlValueSyntax be String => HasSqlValueSyntax be LocationField where
  sqlValueSyntax = autoSqlValueSyntax
instance (IsSql92ColumnSchemaSyntax be) => HasDefaultSqlDataTypeConstraints be LocationField

instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlEqualityCheck be LocationField
instance (HasSqlValueSyntax (Sql92ExpressionValueSyntax be) Bool,
          IsSql92ExpressionSyntax be) =>
          HasSqlQuantifiedEqualityCheck be LocationField

-- | An explicit definition of ``fromBackendRow`` is required for each custom type
instance FromBackendRow Postgres LocationField where
  fromBackendRow = do
    val <- fromBackendRow
    case val :: T.Text of
      "Src" -> pure Src
      "Dest" -> pure Dest
      "BizLocation" -> pure BizLocation
      "ReadPoint" -> pure ReadPoint
      _ -> fail ("Invalid value for LocationField: " ++ T.unpack val)

instance FromField LocationField where
  fromField = defaultFromField "LocationField"

instance ToField LocationField where
  toField = toField . show

locationType :: DataType PgDataTypeSyntax LocationField
locationType = textType
