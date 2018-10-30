{-# LANGUAGE FlexibleContexts #-}

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
import           Database.Beam.Postgres.Syntax        (PgColumnSchemaSyntax,
                                                       PgDataTypeSyntax,
                                                       pgTextType)



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

-- | Field definition to use for last updated columns
lastUpdateField :: BMigrate.TableFieldSchema PgColumnSchemaSyntax (Maybe LocalTime)
lastUpdateField = field "last_update" (maybeType timestamptz) (defaultTo_ (B.just_ now_))

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
