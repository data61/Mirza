{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Mirza.Common.Beam where


import qualified Database.Beam                        as B
import qualified Database.Beam.Backend.SQL            as BSQL
import qualified Database.Beam.Migrate                as BMigrate

import           Database.PostgreSQL.Simple.FromField

import           Data.ByteString                      (ByteString)
import qualified Data.Text                            as T
import           Data.Time                            (LocalTime)
import           Text.Read

import           Database.Beam.Migrate.SQL
import           Database.Beam.Postgres
import           Database.Beam.Postgres.Syntax        (pgTextType)

import           Database.Beam.Query.DataTypes        (DataType (..), maybeType)

import           Data.Text                            (Text)

import           Data.UUID                            (UUID)

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

textType :: DataType Postgres a
textType = DataType pgTextType

pkSerialType :: DataType Postgres UUID
pkSerialType = uuid

-- | Field definition to use for last updated columns
lastUpdateField :: BMigrate.TableFieldSchema Postgres (Maybe LocalTime)
lastUpdateField = field "last_update" (maybeType B.timestamp) (defaultTo_ (B.just_ now_))

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

-- defaultFkConstraint :: Text -> [Text] -> Constraint be
defaultFkConstraint :: IsSql92ColumnConstraintSyntax
                      (Sql92ColumnConstraintDefinitionConstraintSyntax
                        (Sql92ColumnSchemaColumnConstraintDefinitionSyntax
                            (Sql92CreateTableColumnSchemaSyntax
                              (Sql92DdlCommandCreateTableSyntax
                                  (BSQL.BeamSqlBackendSyntax be)))))
                    => Text -> [Text] -> Constraint be
defaultFkConstraint tblName fields =
  Constraint $
    referencesConstraintSyntax tblName fields Nothing Nothing $
      pure referentialActionCascadeSyntax

