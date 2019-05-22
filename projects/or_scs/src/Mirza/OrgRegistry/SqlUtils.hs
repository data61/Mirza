module Mirza.OrgRegistry.SqlUtils
  (
    Utils.handleError
  , transformSqlUniqueViloation
  , handleSqlUniqueViloation
  ) where


import           Mirza.OrgRegistry.Types as ORT
import qualified Mirza.Common.Utils           as Utils

import           Database.PostgreSQL.Simple   (SqlError (..))

import           Control.Lens                 (( # ))

import           Data.ByteString

transformSqlUniqueViloation  :: (AsSqlError err, AsORError err, MonadError err m, MonadIO m)
                          => ByteString        -- ^ UniqueViolation name.
                          -> (SqlError -> err) -- ^ A function which takes the original SQL error for the
                                               --   UniqueViolation and turns it into the error that is thrown
                                               --   when the UniqueViolation name is matched.
                          -> err               -- ^ The error that we are catching.
                          -> m a
transformSqlUniqueViloation = Utils.transformSqlUniqueViloationTemplate (_UnmatchedUniqueViolationORE #)

handleSqlUniqueViloation  :: (AsSqlError err, AsORError err, MonadError err m, MonadIO m)
                           => ByteString        -- ^ UniqueViolation name.
                           -> (SqlError -> m a) -- ^ A function which takes the original SQL error for the
                                                --   UniqueViolation and handles what should happen in this case.
                           -> err               -- ^ The error that we are catching.
                           -> m a
handleSqlUniqueViloation = Utils.handleSqlUniqueViloationTemplate (_UnmatchedUniqueViolationORE #)