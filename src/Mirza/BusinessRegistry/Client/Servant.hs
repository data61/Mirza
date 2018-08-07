module Mirza.BusinessRegistry.Client.Servant
  (
  -- * Public API
   getKey
  ,getKeyInfo
  ,businessList
  -- * Authenticated API
  ,addPublicKey
  ,revokePublicKey
  ) where

import           Mirza.BusinessRegistry.API
import           Mirza.BusinessRegistry.Types as BRT
import           Mirza.Common.Time            (ExpirationTime, RevocationTime)
import           Mirza.Common.Types           (KeyID)

import           Servant.API
import           Servant.Client

import           Data.Proxy                   (Proxy (..))

getKey       :: KeyID -> ClientM PEM_RSAPubKey
getKeyInfo   :: KeyID -> ClientM KeyInfoResponse
businessList :: ClientM [BusinessResponse]

addPublicKey        :: BasicAuthData -> PEM_RSAPubKey -> Maybe ExpirationTime -> ClientM KeyID
revokePublicKey     :: BasicAuthData -> KeyID -> ClientM RevocationTime

_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
        getKey
    :<|>getKeyInfo
    :<|>businessList
  )
  :<|>
  _privAPI@(
         addPublicKey
    :<|> revokePublicKey
  )
 ) = client (Proxy :: Proxy ServerAPI)
