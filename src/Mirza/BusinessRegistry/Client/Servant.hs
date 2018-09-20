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
  , addLocation
  , removeLocation
  ) where

import           Mirza.BusinessRegistry.API
import           Mirza.BusinessRegistry.Types as BRT
import           Mirza.BusinessRegistry.Database.Schema (LocationId)
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
addLocation     :: BasicAuthData -> NewLocation -> ClientM LocationId
removeLocation  :: BasicAuthData -> LocationId -> ClientM NoContent


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
    :<|> addLocation
    :<|> removeLocation
  )
 ) = client (Proxy :: Proxy ServerAPI)
