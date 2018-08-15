module Mirza.BusinessRegistry.Client.Servant
  (
  -- * Public API
    getKey
  , getKeyInfo
  , listBusiness
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

getKey       :: BRKeyId -> ClientM PEM_RSAPubKey
getKeyInfo   :: BRKeyId -> ClientM KeyInfoResponse
listBusiness :: ClientM [BusinessResponse]
addUser      :: NewUser     -> ClientM UserId
addBusiness  :: NewBusiness -> ClientM GS1CompanyPrefix

addPublicKey    :: BasicAuthData -> PEM_RSAPubKey -> Maybe ExpirationTime -> ClientM BRKeyId
revokePublicKey :: BasicAuthData -> BRKeyId -> ClientM RevocationTime

_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
        getKey
    :<|> getKeyInfo
    :<|> listBusiness
    :<|> addUser
    :<|> addBusiness
  )
  :<|>
  _privAPI@(
         addPublicKey
    :<|> revokePublicKey
  )
 ) = client (Proxy :: Proxy ServerAPI)
