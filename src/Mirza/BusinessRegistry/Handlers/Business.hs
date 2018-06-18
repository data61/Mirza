{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.BusinessRegistry.Handlers.Business
  (
    getPublicKey, getPublicKeyInfo
  , revokePublicKey
  , addPublicKey
  , listBusinesses
  ) where


import           Mirza.BusinessRegistry.Handlers.Common
import qualified Mirza.BusinessRegistry.StorageBeam       as SB
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Types


import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           OpenSSL.EVP.PKey                         (SomePublicKey,
                                                           toPublicKey)
import           OpenSSL.PEM                              (readPublicKey,
                                                           writePublicKey)
import           OpenSSL.RSA                              (RSAPubKey, rsaSize)

import           Control.Monad                            (unless, when)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Data.Text                                as T

import           Data.Time.Clock                          (UTCTime,
                                                           getCurrentTime)
import           Data.Time.LocalTime                      (utc, utcToLocalTime)



getPublicKey ::  BRApp context err => KeyID -> AppM context err PEM_RSAPubKey
getPublicKey = notImplemented



getPublicKeyInfo ::  BRApp context err => KeyID -> AppM context err BT.KeyInfo
getPublicKeyInfo = notImplemented


-- select * from Business;
listBusinesses :: BRApp context err => AppM context err [BusinessResponse]
listBusinesses = notImplemented


addPublicKey :: BRApp context err => BT.AuthUser
             -> PEM_RSAPubKey
             -> Maybe ExpirationTime
             -> AppM context err KeyID
addPublicKey = notImplemented


revokePublicKey :: BRApp context err => BT.AuthUser -> KeyID -> AppM context err UTCTime
revokePublicKey = notImplemented
