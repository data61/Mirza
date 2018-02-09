{-# LANGUAGE OverloadedStrings #-}

module ErrorUtils where

import           AppConfig (AppM(..), AppError(..))
import           Errors (ServiceError(..))
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as LBSC8
import           Control.Monad.Except (throwError, MonadError(..))
import           Servant.Server
import           Utils (toText)

appErrToHttpErr :: ServiceError -> Handler a
appErrToHttpErr (EmailExists email) =
  throwError $ err400 {
    errBody = LBSC8.fromChunks $ ["User email ", encodeUtf8 email, " exists"]}
appErrToHttpErr _ = throwError err500 {errBody = "The server did not understand this request."}

-- | Shorthand for throwing a Backend error
throwBackendError :: (Show e) => e -> AppM a
throwBackendError = throwAppError . BackendErr . toText

-- | Shorthand for throwing AppErrors
-- Added because we were doing a lot of it
throwAppError :: ServiceError -> AppM a
throwAppError = throwError . AppError
