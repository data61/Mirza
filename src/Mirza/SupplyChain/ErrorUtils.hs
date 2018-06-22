{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module contains the helper functions that are used in error handling
module Mirza.SupplyChain.ErrorUtils
  ( appErrToHttpErr
  , throwBackendError
  , getSqlErrorCode
  , throwAppError
  , toServerError
  , throwParseError
  -- , throw500Err
  ) where

import           Mirza.Common.Types
import qualified Mirza.Common.Utils                  as U
import           Mirza.SupplyChain.Types

import           Control.Monad.Except                (MonadError (..),
                                                      throwError)
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString.Lazy.Char8          as LBSC8
import           Data.Text.Encoding                  (encodeUtf8)
import           Text.Printf                         (printf)

import           Data.GS1.EPC
import           Database.PostgreSQL.Simple.Internal (SqlError (..))
import           Servant.Server

-- | Takes in a ServiceError and converts it to an HTTP error (eg. err400)
appErrToHttpErr :: ServiceError -> Handler a
appErrToHttpErr (EmailExists _ (EmailAddress email)) =
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
appErrToHttpErr (InvalidRSAKey _) =
  throwError $ err400 {
    errBody = "Failed to parse RSA Public key."
  }
appErrToHttpErr (InvalidRSAKeySize (Expected (Bit expSize)) (Received (Bit recSize))) =
  throwError $ err400 {
    errBody = LBSC8.pack $ printf "Invalid RSA Key size. Expected: %d Bits, Received: %d Bits\n" expSize recSize
  }
appErrToHttpErr (InvalidDigest _) =
  throwError $ err400 {
    errBody = "Invalid Key ID entered."
  }
appErrToHttpErr (ParseError err) =
  throwError $ err400 {
    errBody = LBSC8.append
                  "We could not parse the input provided. Error(s) encountered"
                  (parseFailureToErrorMsg err)
  }
appErrToHttpErr KeyAlreadyRevoked =
  throwError $ err400 { errBody = "Public Key already revoked" }
appErrToHttpErr UnauthorisedKeyAccess =
  throwError $ err403 { errBody = "Not authorised to access this key." }
appErrToHttpErr (AuthFailed _) =
  throwError $ err403 { errBody = "Authentication failed. Invalid username or password." }
appErrToHttpErr (EventPermissionDenied _ _) =
  throwError $ err403 {
    errBody = "User does not own the event."
  }
appErrToHttpErr (UserNotFound (EmailAddress _email)) =
  throwError $ err404 { errBody = "User not found." }
appErrToHttpErr (EmailNotFound (EmailAddress _email)) =
  throwError $ err404 { errBody = "User not found." }
appErrToHttpErr (InvalidRSAKeyInDB _) = generic500err
appErrToHttpErr (InsertionFail _ _email) = generic500err
appErrToHttpErr (BlockchainSendFailed _) = generic500err
appErrToHttpErr (BackendErr _) = generic500err
appErrToHttpErr (DatabaseError _) = generic500err

generic500err :: Handler a
generic500err = throwError err500 {errBody = "Something went wrong"}

-- UNUSED
_throw500Err :: MonadError ServantErr m => LBSC8.ByteString -> m a
_throw500Err bdy = throwError err500 {errBody = bdy}

-- TODO: Some of these might benefit from HasCallStack constraints

-- | Takes in a function that can extract errorcode out of an error, the error
-- itself and constructs a ``ServerError`` with it
toServerError :: Show a => (a -> Maybe ByteString) -> a -> ServerError
toServerError f e = ServerError (f e) (U.toText e)

-- | Shorthand for throwing a Generic Backend error
throwBackendError :: (Show a, MonadError err m, AsServiceError err) => a -> m b
throwBackendError er = throwing _BackendErr $ U.toText er

-- | Shorthand for throwing AppErrors
-- Added because we were doing a lot of it
throwAppError :: (AsServiceError err, MonadError err m) => ServiceError -> m a
throwAppError = throwing _ServiceError

-- | Extracts error code from an ``SqlError``
getSqlErrorCode :: SqlError -> Maybe ByteString
getSqlErrorCode e@SqlError{} = Just $ sqlState e

throwParseError :: AsServiceError err => ParseFailure -> AppM context err a
throwParseError = throwing _ParseError


parseFailureToErrorMsg :: ParseFailure -> LBSC8.ByteString
-- TODO: Include XML Snippet in the error
parseFailureToErrorMsg e = case e of
  InvalidLength -> "The length of one of your URN's is not correct"
  InvalidFormat -> "Incorrectly formatted XML. Possible Causes: Some components \
                   \of the URN missing,Incorrectly structured, Wrong payload"
  InvalidAction -> "Could not parse the Action provided"
  InvalidBizTransaction -> "Could not parse business transaction"
  InvalidEvent -> "Could not parse the event supplied"
  TimeZoneError -> "There was an error in parsing the timezone"
  TagNotFound -> "One or more required tags missing"
  InvalidDispBizCombination -> "The combination of Disposition\
                               \ and Business Transaction is incorrect"
-- TODO: map parseFailureToErrorMsg <all_failures> joined by "\n"
  ChildFailure _ -> "Encountered several errors while parsing the data provided."
