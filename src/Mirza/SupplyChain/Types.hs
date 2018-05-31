{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}



-- | Contains the definition of our ReaderT AppM
module Mirza.SupplyChain.Types
  (mkEnvType
  , SCSContext(..)
  , HasScryptParams(..)
  , AppError(..)
  , EventOwner(..)
  , SigningUser(..)
  , Bit(..)
  , Byte(..)
  , module Common
  -- * Errors
  , ServiceError(..)
  , ServerError(..)
  , AsServiceError(..)
  , Expected(..)
  , Received(..)
  )
  where

-- import           Mirza.SupplyChain.Errors   (ServiceError (..))
import qualified Mirza.SupplyChain.Model    as M

import qualified Data.GS1.EPC               as EPC
import           Data.GS1.EventId           as EvId


import           Database.PostgreSQL.Simple (Connection, SqlError)

import           Crypto.Scrypt              (ScryptParams)

import           Data.Pool                  as Pool

import           GHC.Generics               (Generic)

import qualified Data.ByteString            as BS
import           Data.Text                  (Text)

import           Control.Lens

import           Mirza.Common.Types         as Common


mkEnvType :: Bool -> EnvType
mkEnvType False = Prod
mkEnvType _     = Dev

data SCSContext = SCSContext
  { _envType    :: EnvType
  , _dbConnPool :: Pool Connection
  , _scryptPs   :: ScryptParams
  -- , port    :: Word16
  }

instance HasEnvType SCSContext where
  envType = lens _envType (\scs e' -> scs{_envType = e'} )

instance HasConnPool SCSContext where
  connPool = lens _dbConnPool (\scs p' -> scs{_dbConnPool = p'})

-- | The class of contexts which have Scrypt parameters
class HasScryptParams a where
  scryptParams :: Lens' a ScryptParams

instance HasScryptParams SCSContext where
  scryptParams = lens _scryptPs (\scsc p' -> scsc{_scryptPs = p'})

-- TODO: Document all these types!

newtype EventOwner  = EventOwner M.UserID deriving(Generic, Show, Eq, Read)
newtype SigningUser = SigningUser M.UserID deriving(Generic, Show, Eq, Read)


newtype Bit  = Bit  {unBit :: Int} deriving (Show, Eq, Read, Ord)
newtype Byte = Byte {unByte :: Int} deriving (Show, Eq, Read, Ord)


newtype Expected = Expected {unExpected :: Bit} deriving (Show, Eq, Read, Ord)
newtype Received = Received {unReceived :: Bit} deriving (Show, Eq, Read, Ord)


-- | Top level application error type, which combines errors from several
-- domains. Currently only `ServiceError` is contained by AppError, but as this
-- is broken into smaller error domains and other domains are added more
-- constructors will be added.
newtype AppError = AppError ServiceError deriving (Show)


data ServerError = ServerError (Maybe BS.ByteString) Text
                   deriving (Show, Eq, Generic, Read)

-- | A sum type of errors that may occur in the Service layer
data ServiceError
  = InvalidSignature      String
  | BlockchainSendFailed  ServerError
  | InvalidEventID        EventId
  | InvalidKeyID          M.KeyID
  | InvalidUserID         M.UserID
  | InvalidRSAKeyInDB     Text -- when the key already existing in the DB is wrong
  | InvalidRSAKey         M.PEM_RSAPubKey
  | InvalidRSAKeySize     Expected Received
  | InvalidDigest         M.Digest
  | InsertionFail         ServerError Text
  | EventPermissionDenied M.UserID EvId.EventId
  | EmailExists           ServerError M.EmailAddress
  | EmailNotFound         M.EmailAddress
  | AuthFailed            M.EmailAddress
  | UserNotFound          M.EmailAddress
  | ParseError            EPC.ParseFailure
  | BackendErr            Text -- fallback
  | DatabaseError         SqlError
  deriving (Show, Eq, Generic)

$(makeClassyPrisms ''ServiceError)

instance AsServiceError AppError where
  _ServiceError = prism' AppError (\(AppError se) -> Just se)

instance AsSqlError ServiceError where
  _SqlError = _DatabaseError

instance AsSqlError AppError where
  _SqlError = _DatabaseError
