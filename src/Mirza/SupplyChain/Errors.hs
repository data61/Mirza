{-# LANGUAGE DeriveGeneric #-}

-- | Contains definition(s) of some error types
module Mirza.SupplyChain.Errors where

import qualified Mirza.SupplyChain.Model    as M
import qualified Mirza.SupplyChain.Utils    as U

import qualified Data.ByteString            as BS
import           Data.GS1.EventID           as EvId
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (SqlError)
import           GHC.Generics               (Generic)

type ErrorText = T.Text
type ErrorCode = BS.ByteString

data ServerError = ServerError (Maybe ErrorCode) ErrorText
                   deriving (Show, Eq, Generic, Read)

newtype Expected = Expected  {unExpected :: U.Byte} deriving (Show, Eq, Read)
newtype Received = Received  {unReceived :: U.Byte} deriving (Show, Eq, Read)

-- | A sum type of errors that may occur in the Service layer
data ServiceError
  = NeedMoreSignatures    T.Text
  | InvalidSignature      String
  | BlockchainSendFailed  ServerError
  | InvalidEventID        EventID
  | InvalidKeyID          M.KeyID
  | InvalidUserID         M.UserID
  | InvalidRSAKeyString   T.Text
  | InvalidRSAKey         M.PEM_RSAPubKey
  | InvalidRSAKeySize     Expected Received
  | InvalidDigest         M.Digest
  | InsertionFail         ServerError T.Text
  | EmailExists           ServerError M.EmailAddress
  | EmailNotFound         M.EmailAddress
  | EventPermissionDenied M.UserID EvId.EventID
  | AuthFailed            M.EmailAddress
  | UserNotFound          M.EmailAddress
  | ParseError            ErrorText -- EPC.ParseFailure
  | BackendErr            ErrorText -- fallback
  | DatabaseError         SqlError
  deriving (Show, Eq, Generic)
