{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.BusinessRegistry.Handlers.Keys
  (
    getPublicKey
  , getPublicKeyInfo
  , revokePublicKey
  , addPublicKey
  ) where


import           Mirza.BusinessRegistry.Handlers.Common
import           Mirza.BusinessRegistry.Types           as BT
import           Mirza.Common.Types
import           Mirza.Common.Utils

--import           Database.Beam                            as B
--import           Database.Beam.Backend.SQL.BeamExtensions

import           Data.Time.Clock                        (UTCTime)


getPublicKey ::  BRApp context err => KeyID -> AppM context err PEM_RSAPubKey
getPublicKey = notImplemented


getPublicKeyInfo ::  BRApp context err => KeyID -> AppM context err BT.KeyInfo
getPublicKeyInfo = notImplemented


addPublicKey :: BRApp context err => BT.AuthUser
             -> PEM_RSAPubKey
             -> Maybe ExpirationTime
             -> AppM context err KeyID
addPublicKey = notImplemented


revokePublicKey :: BRApp context err => BT.AuthUser -> KeyID -> AppM context err UTCTime
revokePublicKey = notImplemented
