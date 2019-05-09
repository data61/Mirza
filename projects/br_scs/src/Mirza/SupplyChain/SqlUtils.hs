module Mirza.SupplyChain.SqlUtils
  (
    Utils.handleError
  , transformSqlUniqueViloation
  ) where


import qualified Mirza.Common.Utils         as Utils
import           Mirza.SupplyChain.Types    as ST

import           Database.PostgreSQL.Simple (SqlError (..))

import           Control.Lens               (( # ))

import           Data.ByteString            (ByteString)


transformSqlUniqueViloation  :: (AsSqlError err, AsServiceError err, MonadError err m, MonadIO m)
                          => ByteString        -- ^ UniqueViolation name.
                          -> (SqlError -> err) -- ^ A function which takes the original SQL error for the
                                               --   UniqueViolation and turns it into the error that is thrown
                                               --   when the UniqueViolation name is matched.
                          -> err               -- ^ The error that we are catching.
                          -> m a
transformSqlUniqueViloation = Utils.transformSqlUniqueViloationTemplate (_UnmatchedUniqueViolation #)
