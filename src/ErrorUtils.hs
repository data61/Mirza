{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
appErrToHttpErr (InvalidKeyID _) =
  throwError $ err400 {
    errBody = "Invalid Key ID entered."
  }
appErrToHttpErr (NeedMoreSignatures _) =
  throwError $ err400 {
    errBody = "We need more signatures."
  }
appErrToHttpErr (InvalidSignature _) =
  throwError $ err400 {
    errBody = "Invalid Signature entered."
  }
appErrToHttpErr (InvalidEventID _) =
  throwError $ err400 {
    errBody = "Invalid Event ID entered."
  }
appErrToHttpErr (InvalidUserID _) =
  throwError $ err400 {
    errBody = "Invalid User ID entered."
  }
appErrToHttpErr (InvalidRSAKeyString _) =
  throwError $ err400 {
    errBody = "Invalid RSA Key entered."
  }
appErrToHttpErr (InvalidRSAKey _) =
  throwError $ err400 {
    errBody = "Invalid RSA Key entered."
  }
appErrToHttpErr (InvalidRSAKeySize _ _) =
  throwError $ err400 {
    errBody = "Invalid RSA Key entered."
  }
appErrToHttpErr (InvalidDigest _) =
  throwError $ err400 {
    errBody = "Invalid Key ID entered."
  }
appErrToHttpErr (AuthFailed (M.EmailAddress email)) =
  throwError $ err403 {
    errBody = LBSC8.fromChunks ["Authentication failed for email ", encodeUtf8 email, "."]
  }
appErrToHttpErr (UserNotFound (M.EmailAddress email)) =
  throwError $ err404 {
    errBody = LBSC8.fromChunks ["User with email ", encodeUtf8 email, " could not be found."]
  }
appErrToHttpErr (EmailNotFound (M.EmailAddress email)) =
  throwError $ err404 {
    errBody = LBSC8.fromChunks ["Email ", encodeUtf8 email, " could not be found."]
  }
appErrToHttpErr (InsertionFail _ email) =
  throw500Err $ LBSC8.fromChunks ["Email ", encodeUtf8 email, " could not be added. It might be worth trying again."]
appErrToHttpErr (BlockchainSendFailed _) =
  throw500Err "The data failed to send to the blockchain. We are looking into it. Thank you for your patience."
appErrToHttpErr (ParseError _) =
  throwError $ err400 {
    errBody = "We could not parse the input provided."
    -- TODO: ^ Add more information on what's wrong?
  }
appErrToHttpErr (BackendErr _) = generic500err
appErrToHttpErr (DatabaseError _) =
  throw500Err "Something went wrong."
-- TODO: The above error messages may need to be more descriptive

generic500err :: Handler a
generic500err = throwError err500 {errBody = "The server did not understand this request."}

throw500Err :: MonadError ServantErr m => LBSC8.ByteString -> m a
throw500Err bdy = throwError err500 {errBody = bdy}

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

throwParseError :: ParseFailure -> AppM a
throwParseError = throwAppError . ParseError . toText

