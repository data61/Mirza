{-# LANGUAGE DeriveGeneric #-}

-- | Contains definition(s) of some error types
module Mirza.SupplyChain.Errors
  ( ServiceError(..)
  , ServerError(..)
  , Expected(..)
  , Received(..)
  , ErrorCode
  ) where

import qualified Mirza.SupplyChain.Model    as M
import qualified Mirza.SupplyChain.Utils    as U

import qualified Data.GS1.EPC               as EPC
import           Data.GS1.EventID           as EvId

import qualified Data.ByteString            as BS
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (SqlError)
import           GHC.Generics               (Generic)

type ErrorCode = BS.ByteString

data ServerError = ServerError (Maybe ErrorCode) Text
                   deriving (Show, Eq, Generic, Read)

newtype Expected = Expected {unExpected :: U.Byte} deriving (Show, Eq, Read)
newtype Received = Received {unReceived :: U.Byte} deriving (Show, Eq, Read)

-- | A sum type of errors that may occur in the Service layer
data ServiceError
  = InvalidSignature      String
  | BlockchainSendFailed  ServerError
  | InvalidEventID        EventID
  | InvalidKeyID          M.KeyID
  | InvalidUserID         M.UserID
  | InvalidRSAKeyInDB     T.Text -- when the key already existing in the DB is wrong
  | InvalidRSAKey         M.PEM_RSAPubKey
  | InvalidRSAKeySize     Expected Received
  | InvalidDigest         M.Digest
  | InsertionFail         ServerError T.Text
  | EventPermissionDenied M.UserID EvId.EventID
  | EmailExists           ServerError M.EmailAddress
  | EmailNotFound         M.EmailAddress
  | AuthFailed            M.EmailAddress
  | UserNotFound          M.EmailAddress
  | ParseError            EPC.ParseFailure
  | BackendErr            Text -- fallback
  | DatabaseError         SqlError
  deriving (Show, Eq, Generic)
