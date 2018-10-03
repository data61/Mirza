module Mirza.BusinessRegistry.SqlUtils where



import           Mirza.BusinessRegistry.Types             as BT

import           Database.PostgreSQL.Simple               (SqlError(..))
import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (UniqueViolation),
                                                           constraintViolation)

import           Control.Lens                             ((^?))

import           Control.Monad.Except                     (catchError, throwError)

import           Data.ByteString



handleError :: MonadError err m => (err -> m a) -> m a -> m a
handleError = flip catchError

handleSqlUniqueViloation  :: (AsSqlError err, AsBusinessRegistryError err, MonadError err m, MonadIO m)
                          => ByteString        -- ^ UniqueViolation name.
                          -> (SqlError -> err) -- ^ A function which takes the original SQL error for the
                                               --   UniqueViolation and turns it into the error that is thrown
                                               --   when the UniqueViolation name is matched.
                          -> err               -- ^ The error that we are catching.
                          -> m a
handleSqlUniqueViloation expectedName uniqueViolationErrror e = case e ^? _SqlError of
  Nothing -> throwError e
  Just sqlError ->
    case constraintViolation sqlError of
      Just (UniqueViolation violationName)
        | violationName == expectedName -> throwError (uniqueViolationErrror sqlError)
        | otherwise -> throwing _UnmatchedUniqueViolationBRE sqlError
      _ -> throwError e