{-# LANGUAGE FlexibleContexts #-}

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

-- import           Data.GS1.Event                    as Ev

import           Mirza.Common.Types

import           Control.Lens                      ((^?))

import           Control.Monad.Except              (MonadError, catchError,
                                                    throwError)

import           Data.ByteString                   (ByteString)

import           Database.Beam.Postgres            (PgJSON (..))

-- | Converts anything to a ``Text``
toText :: Show a => a -> T.Text
toText = T.pack . show

randomText :: IO T.Text
randomText = do
  count <- randomRIO (8 :: Int, 32)
  randomString <- (take count) <$> replicateM count (randomRIO ('a', 'z'))
  return $ T.pack randomString

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
