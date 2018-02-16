{-# LANGUAGE OverloadedStrings #-}

module ErrorUtils where

import           AppConfig (AppM(..), AppError(..))
import           Errors (ServiceError(..), ServerError(..), ErrorCode)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import           Control.Monad.Except (throwError, MonadError(..))
import           Servant.Server
import           Utils (toText)
import           Database.PostgreSQL.Simple.Internal (SqlError(..))
import           Data.ByteString (ByteString(..))

appErrToHttpErr :: ServiceError -> Handler a
appErrToHttpErr (EmailExists _ email) =
  throwError $ err400 {
    errBody = LBSC8.fromChunks $ ["User email ", encodeUtf8 email, " exists."]
  }
appErrToHttpErr (UnexpectedDBResponse _) =
  throwError $ err500 {
    errBody = "We received an unexpected response from our database. This error has been logged and someone is looking into it."
  }
appErrToHttpErr (UserNotFound email) =
  throwError $ err404 {
    errBody = LBSC8.fromChunks $ ["User with email ", encodeUtf8 email, " could not be found."]
  }
appErrToHttpErr (AuthFailed email) =
  throwError $ err404 {
    errBody = LBSC8.fromChunks $ ["Authentication failed for email ", encodeUtf8 email, "."]
  }
appErrToHttpErr (EmailNotFound email) =
  throwError $ err404 {
    errBody = LBSC8.fromChunks $ ["Email ", encodeUtf8 email, " could not be found."]
  }
appErrToHttpErr (InsertionFail _ email) =
  throwError $ err500 {
    errBody = LBSC8.fromChunks $ ["Email ", encodeUtf8 email, " could not be inserted."]
  }
appErrToHttpErr (InvalidKeyID _) =
  throwError $ err400 {
    errBody = LBSC8.fromChunks $ ["Invalid Key ID entered."]
  }

appErrToHttpErr _ = throwError err500 {errBody = "The server did not understand this request."}


toServerError :: Show a => (a -> Maybe ErrorCode) -> a -> ServerError
toServerError f e = ServerError (f e) (toText e)

defaultToServerError :: Show a => a -> ServerError
defaultToServerError = toServerError (const Nothing)

sqlToServerError :: SqlError -> ServerError
sqlToServerError = toServerError getSqlErrorCode

-- | Shorthand for throwing a Backend error
throwBackendError :: (Show a) => a -> AppM b
throwBackendError er = throwError $ AppError $ BackendErr $ toText er

-- | Shorthand for throwing AppErrors
-- Added because we were doing a lot of it
throwAppError :: ServiceError -> AppM a
throwAppError = throwError . AppError

getSqlErrorCode :: SqlError -> Maybe ByteString
getSqlErrorCode e@(SqlError _ _ _ _ _) = Just $ sqlState e


throwUnexpectedDBError :: ServerError -> AppM a
throwUnexpectedDBError = throwAppError . UnexpectedDBResponse
