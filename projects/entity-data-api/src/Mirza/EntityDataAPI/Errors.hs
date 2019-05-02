{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Mirza.EntityDataAPI.Errors where

import           Control.Lens                      (makeClassyPrisms, prism')

import           GHC.Generics                      (Generic)

import           Database.PostgreSQL.Simple        (SqlError (..))

import           Crypto.JWT                        (AsError, AsJWTError,
                                                    StringOrURI)
import qualified Crypto.JWT                        as Jose

import           Network.HTTP.Req                  (HttpException)


import           Database.PostgreSQL.Simple.Errors (ConstraintViolation (..))

data DBError
  = SqlErr SqlError
  | DBConstraintFailed ConstraintViolation
  | InsertionFailed
  | UnauthorisedInsertionAttempt StringOrURI
  deriving (Eq, Show, Generic)

data AppError
  = JWKFetchFailed
  | AuthFailed Jose.JWTError
  | AppJoseError Jose.Error
  | NoClaimSubject
  | UnauthClaimsSubject
  | NoAuthHeader
  | UrlParseFailed
  | ReqFailure HttpException
  | JWKParseFailure String
  | DatabaseError DBError
  deriving (Show, Generic)
makeClassyPrisms ''AppError

instance AsJWTError AppError where
  _JWTError = prism' AuthFailed
              (\case
                (AuthFailed e) -> Just e
                _              -> Nothing
              )
instance AsError AppError where
  _Error = _AppJoseError

