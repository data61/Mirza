{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module contains the helper functions that are used in error handling
module ErrorUtils where

import           AppConfig                           (AppError (..), AppM)
import           Control.Monad.Except                (MonadError (..),
                                                      throwError)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy.Char8          as LBSC8
import           Data.Text.Encoding                  (encodeUtf8)
import           Text.Printf                         (printf)

import           Data.GS1.EPC
import           Database.PostgreSQL.Simple.Internal (SqlError (..))
import           Errors                              (ErrorCode, Expected (..),
                                                      Received (..),
                                                      ServerError (..),
                                                      ServiceError (..))
import qualified Model                               as M
import           Servant.Server
import qualified Utils                               as U

-- | Takes in a ServiceError and converts it to an HTTP error (eg. err400)
appErrToHttpErr :: ServiceError -> Handler a
appErrToHttpErr (EmailExists _ (M.EmailAddress email)) =
  throwError $ err400 {
    errBody = LBSC8.fromChunks ["User email ", encodeUtf8 email, " exists."]
  }
appErrToHttpErr (InvalidKeyID _) =
  throwError $ err400 {
    errBody = "Invalid Key ID entered."
  }
appErrToHttpErr (InvalidSignature _) =
  throwError $ err400 {
    errBody = "Invalid Signature entered."
  }
appErrToHttpErr (InvalidEventID _) =
  throwError $ err400 {
    errBody = "No such event."
  }
appErrToHttpErr (InvalidUserID _) =
  throwError $ err400 {
    errBody = "No such user."
  }
appErrToHttpErr (InvalidRSAKeyString _) =
  throwError $ err400 {
    errBody = "Invalid RSA Key entered."
  }
appErrToHttpErr (InvalidRSAKey _) =
  throwError $ err400 {
    errBody = "Failed to parse RSA Public key."
  }
appErrToHttpErr (InvalidRSAKeySize (Expected (U.Byte expSize)) (Received (U.Byte recSize))) =
  throwError $ err400 {
    errBody = LBSC8.pack $ printf "Invalid RSA Key size. Expected: %d, Received: %d\n" expSize recSize
  }
appErrToHttpErr (InvalidDigest _) =
  throwError $ err400 {
    errBody = "Invalid Key ID entered."
  }
appErrToHttpErr (ParseError _) =
  throwError $ err400 {
    errBody = "We could not parse the input provided."
    -- TODO: ^ Add more information on what's wrong?
  }
appErrToHttpErr (AuthFailed _) =
  throwError $ err403 { errBody = "Authentication failed." }
appErrToHttpErr (UserNotFound (M.EmailAddress _email)) =
  throwError $ err404 { errBody = "User not found." }
appErrToHttpErr (EmailNotFound (M.EmailAddress _email)) =
  throwError $ err404 { errBody = "User not found." }
appErrToHttpErr (InsertionFail _ _email) = generic500err
appErrToHttpErr (BlockchainSendFailed _) = generic500err
appErrToHttpErr (BackendErr _) = generic500err
appErrToHttpErr (DatabaseError _) = generic500err
-- TODO: The above error messages may need to be more descriptive

generic500err :: Handler a
generic500err = throwError err500 {errBody = "Something went wrong"}

throw500Err :: MonadError ServantErr m => LBSC8.ByteString -> m a
throw500Err bdy = throwError err500 {errBody = bdy}

-- TODO: Some of these might benefit from HasCallStack constraints

-- | Takes in a function that can extract errorcode out of an error, the error
-- itself and constructs a ``ServerError`` with it
toServerError :: Show a => (a -> Maybe ErrorCode) -> a -> ServerError
toServerError f e = ServerError (f e) (U.toText e)

-- | Shorthand for ``toServerError``.
-- Use if you can't think of a function to extract the error code
defaultToServerError :: Show a => a -> ServerError
defaultToServerError = toServerError (const Nothing)

-- | Shorthand for only SqlError types
sqlToServerError :: SqlError -> ServiceError
sqlToServerError = DatabaseError -- toServerError getSqlErrorCode

-- | Shorthand for throwing a Generic Backend error
throwBackendError :: (Show a) => a -> AppM b
throwBackendError er = throwError $ AppError $ BackendErr $ U.toText er

-- | Shorthand for throwing AppErrors
-- Added because we were doing a lot of it
throwAppError :: ServiceError -> AppM a
throwAppError = throwError . AppError

-- | Extracts error code from an ``SqlError``
getSqlErrorCode :: SqlError -> Maybe ByteString
getSqlErrorCode e@(SqlError{}) = Just $ sqlState e

throwParseError :: ParseFailure -> AppM a
throwParseError = throwAppError . ParseError . U.toText
