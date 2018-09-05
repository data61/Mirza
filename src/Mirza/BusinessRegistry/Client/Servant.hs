module Mirza.BusinessRegistry.Client.Servant
  (
  -- * Public API
    getPublicKey
  , getPublicKeyInfo
  , listBusinesses
  , addUser
  , addBusiness
  -- * Authenticated API
  , addPublicKey
  , revokePublicKey
  ) where

import           Mirza.BusinessRegistry.API
import           Mirza.BusinessRegistry.Types as BRT
import           Mirza.Common.Time            (ExpirationTime, RevocationTime)
import           Mirza.Common.Types           (BRKeyId)

import           Data.GS1.EPC                 as EPC

import           Servant.API
import           Servant.Client

import           Data.Proxy                   (Proxy (..))

getPublicKey       :: BRKeyId -> ClientM PEM_RSAPubKey
getPublicKeyInfo   :: BRKeyId -> ClientM KeyInfoResponse
listBusinesses :: ClientM [BusinessResponse]

addUser         :: BasicAuthData -> NewUser     -> ClientM UserId
addBusiness     :: BasicAuthData -> NewBusiness -> ClientM GS1CompanyPrefix
addPublicKey    :: BasicAuthData -> PEM_RSAPubKey -> Maybe ExpirationTime -> ClientM BRKeyId
revokePublicKey :: BasicAuthData -> BRKeyId -> ClientM RevocationTime

_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         getPublicKey
    :<|> getPublicKeyInfo
    :<|> listBusinesses
  )
  :<|>
  _privAPI@(
         addUser
    :<|> addBusiness
    :<|> addPublicKey
    :<|> revokePublicKey
  )
 ) = client (Proxy :: Proxy ServerAPI)
