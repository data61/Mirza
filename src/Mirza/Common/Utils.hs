{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Utils
  (
    toText
  , randomText
  , notImplemented
  , newUUID
  , handleError
  , handleSqlUniqueViloationTemplate
  , fromPgJSON
  , addLastUpdateTriggers
  ) where


import           Control.Monad                     (replicateM)
import           Control.Monad.IO.Class            (MonadIO, liftIO)

import qualified Data.Text                         as T
import           Data.UUID                         (UUID)
import           Data.UUID.V4                      (nextRandom)

import           System.Random                     (randomRIO)

import           GHC.Stack                         (HasCallStack)

import           Database.PostgreSQL.Simple        (SqlError (..))
import           Database.PostgreSQL.Simple.Errors (ConstraintViolation (UniqueViolation),
                                                    constraintViolation)

import           Mirza.Common.Types

import           Control.Lens                      (view, (^.), (^?), _1)

import           Control.Monad.Except              (MonadError, catchError,
                                                    throwError)

import           Data.ByteString                   (ByteString)

import           Database.Beam.Postgres            (PgJSON (..), Postgres)


import           Control.Monad.Writer
import           Data.Functor                      ((<$))
import           Data.Proxy                        (Proxy (..))
import           Data.String                       (fromString)
import           Data.Text                         (Text, unpack)
import           Database.Beam.Schema.Tables
import           Database.PostgreSQL.Simple        (execute_)
import           Katip


-- | Converts anything to a ``Text``
toText :: Show a => a -> T.Text
toText = T.pack . show

randomText :: IO T.Text
randomText = do
  count <- randomRIO (8 :: Int, 32)
  randomString <- (take count) <$> replicateM count (randomRIO ('a', 'z'))
  pure $ T.pack randomString

{-# WARNING notImplemented "notImplemented should not be used" #-}
notImplemented :: HasCallStack => a
notImplemented = error "FIXME"

-- | Generate a new v4 UUID - when being used in a database transaction,
-- this should be called inside the DB monad so that if the transaction
-- is retried a new UUID will be generated.
newUUID :: MonadIO m => m UUID
newUUID = liftIO nextRandom

-- | Ueful for handling specific errors from, for example, database transactions
-- @
--  handleError errHandler $ runDb ...
--  ...
--  where errHandler (AppError (DatabaseError sqlErr)) = ...
--        errHandler e = throwError e
-- @
handleError :: MonadError err m => (err -> m a) -> m a -> m a
handleError = flip catchError

handleSqlUniqueViloationTemplate  :: (AsSqlError err, MonadError err m, MonadIO m)
                          => (SqlError -> err) -- ^ Handles every other unique constraint violation
                          -> ByteString        -- ^ UniqueViolation name.
                          -> (SqlError -> err) -- ^ A function which takes the original SQL error for the
                                               --   UniqueViolation and turns it into the error that is thrown
                                               --   when the UniqueViolation name is matched.
                          -> err               -- ^ The error that we are catching.
                          -> m a
handleSqlUniqueViloationTemplate f expectedName uniqueViolationError e = case e ^? _SqlError of
  Nothing -> throwError e
  Just sqlError ->
    case constraintViolation sqlError of
      Just (UniqueViolation violationName)
        | violationName == expectedName -> throwError (uniqueViolationError sqlError)
        | otherwise -> throwError (f sqlError)
      _ -> throwError e

fromPgJSON :: PgJSON a -> a
fromPgJSON (PgJSON x) = x



getTableNames :: Database Postgres db => DatabaseSettings Postgres db -> [Text]
getTableNames db = execWriter $ zipTables (Proxy :: Proxy Postgres)
    (\(DatabaseEntity desc) _ -> undefined <$ tell [desc ^. dbEntityName])
    db
    db

-- Adds triggers to all tables to set the last_update field to NOW(); on
-- INSERT and UPDATE.
-- See: https://stackoverflow.com/q/8740792
addLastUpdateTriggers :: (HasLogging context
                         ,Database Postgres db)
                      => DatabaseSettings Postgres db
                      -> DB context err ()
addLastUpdateTriggers db = forM_ (getTableNames db) $ \tName -> do
  conn <- view _1
  $(logTM) InfoS . logStr $ "Adding triggers to: " <> tName
  liftIO $ execute_ conn $ fromString $ unpack $
    "CREATE OR REPLACE FUNCTION sync_lastmod() RETURNS trigger AS $$ \
      \BEGIN \
        \NEW.last_update := NOW(); \
        \RETURN NEW; \
      \END; \
      \$$ LANGUAGE plpgsql; \
      \DROP TRIGGER IF EXISTS sync_lastmod ON \"" <> tName <> "\";" <>
      "CREATE TRIGGER sync_lastmod \
      \BEFORE UPDATE OR INSERT ON \"" <> tName <>
        "\" FOR EACH ROW EXECUTE PROCEDURE sync_lastmod();"
