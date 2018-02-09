{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module Errors where

import qualified Model as M
import           GHC.Generics (Generic)
import qualified Data.Text as T
import qualified Data.ByteString as BS

type ErrorText = T.Text
-- type ErrorCode = Int
-- data ServerError = ServerError (Maybe ErrorCode) ErrorText

-- | A sum type of errors that may occur in the Service layer
data ServiceError = NeedMoreSignatures T.Text
                  | InvalidSignature BS.ByteString
                  | BlockchainSendFailed ErrorText
                  | InvalidEventID Int
                  | InvalidKeyID M.KeyID
                  | InvalidUserID M.UserID
                  | InsertionFail ErrorText
                  | EmailExists M.Email
                  | EmailNotFound M.Email
                  | BackendErr ErrorText
                  | UnexpectedDBResponse ErrorText
                  | AuthFailed M.Email
                  | UserNotFound M.Email
                  deriving (Show, Read, Generic)

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


