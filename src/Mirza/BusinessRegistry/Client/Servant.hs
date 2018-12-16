module Mirza.BusinessRegistry.Client.Servant
  (
  -- * Public API
    getPublicKey
  , getPublicKeyInfo
  , searchBusinesses
  , health
  , addUser
  , addBusiness
  -- * Authenticated API
  , addPublicKey
  , revokePublicKey
  , addLocation
  , getLocationByGLN
  , searchLocation
  ) where

import           Mirza.BusinessRegistry.API
import           Mirza.BusinessRegistry.Database.Schema (LocationId)
import           Mirza.BusinessRegistry.Types           as BRT
import           Mirza.Common.Time                      (ExpirationTime,
                                                         RevocationTime)
import           Mirza.Common.Types                     (BRKeyId)

import           Data.GS1.EPC                           as EPC

import           Crypto.JOSE.JWK                        (JWK)

import           Servant.API
import           Servant.Client

import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           Data.Time                              (UTCTime)

health         :: ClientM HealthResponse
getPublicKey     :: BRKeyId -> ClientM JWK
getPublicKeyInfo :: BRKeyId -> ClientM KeyInfoResponse
searchBusinesses   :: Maybe GS1CompanyPrefix -> Maybe Text -> Maybe UTCTime -> ClientM [BusinessResponse]

addUser          :: BasicAuthData -> NewUser     -> ClientM UserId
addBusiness      :: BasicAuthData -> NewBusiness -> ClientM GS1CompanyPrefix
addPublicKey     :: BasicAuthData -> JWK -> Maybe ExpirationTime -> ClientM BRKeyId
revokePublicKey  :: BasicAuthData -> BRKeyId -> ClientM RevocationTime
addLocation      :: BasicAuthData -> NewLocation -> ClientM LocationId
getLocationByGLN :: BasicAuthData -> LocationEPC -> ClientM LocationResponse
searchLocation   :: BasicAuthData -> Maybe GS1CompanyPrefix -> Maybe UTCTime -> ClientM [LocationResponse] 


_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         health
    :<|> getPublicKey
    :<|> getPublicKeyInfo
    :<|> searchBusinesses
  )
  :<|>
  _privAPI@(
         addUser
    :<|> addBusiness
    :<|> addPublicKey
    :<|> revokePublicKey
    :<|> addLocation
    :<|> getLocationByGLN
    :<|> searchLocation
  )
 ) = client (Proxy :: Proxy ServerAPI)
