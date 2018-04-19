
-- | This module contains the helper functions that are used in error handling
module ErrorUtils where

import           AppConfig                           (AppError (..), AppM)
import           Control.Monad.Except                (MonadError (..),
                                                      throwError)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy.Char8          as LBSC8
import           Data.GS1.EPC
import           Data.Text.Encoding                  (encodeUtf8)
import           Database.PostgreSQL.Simple.Internal (SqlError (..))
import           Errors                              (ErrorCode,
                                                      ServerError (..),
                                                      ServiceError (..))
import qualified Model                               as M
import           Servant.Server
import           Utils                               (toText)

-- | Takes in a ServiceError and converts it to an HTTP error (eg. err400)
appErrToHttpErr :: ServiceError -> Handler a
appErrToHttpErr (EmailExists _ (M.EmailAddress email)) =
  throwError $ err400 {
    errBody = LBSC8.fromChunks ["User email ", encodeUtf8 email, " exists."]
  }
appErrToHttpErr (UnexpectedDBResponse _) =
  throwError $ err500 {
    errBody = "We received an unexpected response from our database. This error has been logged and someone is looking into it."
  }
appErrToHttpErr (UserNotFound (M.EmailAddress email)) =
  throwError $ err404 {
    errBody = LBSC8.fromChunks ["User with email ", encodeUtf8 email, " could not be found."]
  }
appErrToHttpErr (AuthFailed (M.EmailAddress email)) =
  throwError $ err404 {
    errBody = LBSC8.fromChunks ["Authentication failed for email ", encodeUtf8 email, "."]
  }
appErrToHttpErr (EmailNotFound (M.EmailAddress email)) =
  throwError $ err404 {
    errBody = LBSC8.fromChunks ["Email ", encodeUtf8 email, " could not be found."]
  }
appErrToHttpErr (InsertionFail _ email) =
  throwError $ err500 {
    errBody = LBSC8.fromChunks ["Email ", encodeUtf8 email, " could not be inserted."]
  }
appErrToHttpErr (InvalidKeyID _) =
  throwError $ err400 {
    errBody = "Invalid Key ID entered."
  }
appErrToHttpErr (NeedMoreSignatures _) = generic500err
appErrToHttpErr (InvalidSignature _) = generic500err
appErrToHttpErr (BlockchainSendFailed _) = generic500err
appErrToHttpErr (InvalidEventID _) = generic500err
appErrToHttpErr (InvalidUserID _) = generic500err
appErrToHttpErr (InvalidRSAKeyString _) = generic500err
appErrToHttpErr (InvalidRSAKey _) = generic500err
appErrToHttpErr (InvalidRSAKeySize _ _) = generic500err
appErrToHttpErr (InvalidDigest _) = generic500err
appErrToHttpErr (ParseError _) = generic500err
appErrToHttpErr (BackendErr _) = generic500err
appErrToHttpErr (DatabaseError _) = generic500err

-- TODO: We should probably explicitly handle all service errors, removing this lets
-- GHC tell us when we haven't
-- appErrToHttpErr _ = throwError err500 {errBody = "The server did not understand this request."}

generic500err :: Handler a
generic500err = throwError err500 {errBody = "The server did not understand this request."}

-- TODO: Some of these might benefit from HasCallStack constraints

-- | Takes in a function that can extract errorcode out of an error, the error
-- itself and constructs a ``ServerError`` with it
toServerError :: Show a => (a -> Maybe ErrorCode) -> a -> ServerError
toServerError f e = ServerError (f e) (toText e)

-- | Shorthand for ``toServerError``.
-- Use if you can't think of a function to extract the error code
defaultToServerError :: Show a => a -> ServerError
defaultToServerError = toServerError (const Nothing)

-- | Shorthand for only SqlError types
sqlToServerError :: SqlError -> ServiceError
sqlToServerError = DatabaseError -- toServerError getSqlErrorCode

-- | Shorthand for throwing a Generic Backend error
throwBackendError :: (Show a) => a -> AppM b
throwBackendError er = throwError $ AppError $ BackendErr $ toText er

-- | Shorthand for throwing AppErrors
-- Added because we were doing a lot of it
throwAppError :: ServiceError -> AppM a
throwAppError = throwError . AppError

-- | Extracts error code from an ``SqlError``
getSqlErrorCode :: SqlError -> Maybe ByteString
getSqlErrorCode e@(SqlError{}) = Just $ sqlState e

-- | Shorthand for throwing ``UnexpectedDBError``
throwUnexpectedDBError :: ServerError -> AppM a
throwUnexpectedDBError = throwAppError . UnexpectedDBResponse

throwParseError :: ParseFailure -> AppM a
throwParseError = throwAppError . ParseError . toText

