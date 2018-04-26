{-# LANGUAGE DeriveGeneric #-}

-- | Contains definition(s) of some error types
module Errors where

import qualified Data.ByteString            as BS
import           Data.GS1.EventID
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple (SqlError)
import           GHC.Generics               (Generic)
import qualified Model                      as M
import qualified Utils                      as U

type ErrorText = T.Text
type ErrorCode = BS.ByteString

data ServerError = ServerError (Maybe ErrorCode) ErrorText
                   deriving (Show, Eq, Generic, Read)

newtype Expected = Expected {unExpected :: U.Byte} deriving (Show, Eq, Read)
newtype Received = Received {unReceived :: U.Byte} deriving (Show, Eq, Read)

-- | A sum type of errors that may occur in the Service layer
data ServiceError
  = InvalidSignature     String
  | BlockchainSendFailed ServerError
  | InvalidEventID       EventID
  | InvalidKeyID         M.KeyID
  | InvalidUserID        M.UserID
  | InvalidRSAKeyString  T.Text
  | InvalidRSAKey        M.PEM_RSAPubKey
  | InvalidRSAKeySize    Expected Received
  | InvalidDigest        M.Digest
  | InsertionFail        ServerError T.Text
  | EmailExists          ServerError M.EmailAddress
  | EmailNotFound        M.EmailAddress
  | AuthFailed           M.EmailAddress
  | UserNotFound         M.EmailAddress
  | ParseError           ErrorText -- EPC.ParseFailure
  | BackendErr           ErrorText -- fallback
  | DatabaseError        SqlError
  deriving (Show, Eq, Generic)

{-
Do not remove the following commented out code until explicitly asked to
They serve as reference to what the errors used to be before they
were merged into ``ServiceError``
-}
-- -- Interface for converting custom errors to ServantErr
-- class AppServantError err where
--   toServantErr :: err -> ServantErr

-- data SigError = SE_NeedMoreSignatures T.Text
--               | SE_InvalidSignature BS.ByteString
--               | SE_InvalidUser T.Text
--               | SE_BlockchainSendFailed
--               | SE_InvalidEventID Int
--               | SE_InvalidKeyID
--               deriving (Show, Read, Generic)

-- instance AppServantError SigError where
--   toServantErr e = err500 {errBody = LBSC8.pack $ show e}


-- data GetPropertyError = KE_InvalidKeyID
--                       | KE_InvalidUserID
--                       deriving (Show, Read, Generic)

-- instance AppServantError GetPropertyError where
--   toServantErr e = err500 {errBody = LBSC8.pack $ show e}

-- data DBError = DBE_InsertionFail
--              | DBE_EmailExists
--              deriving (Show, Read, Generic)

-- instance AppServantError DBError where
--   toServantErr e = err500 {errBody = LBSC8.pack $ show e}
