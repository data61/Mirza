module Mirza.BusinessRegistry.Client.Servant
  (
  -- * Public API
    health
  , versionInfo
  , getPublicKey
  , getPublicKeyInfo
  , searchBusinesses
  , getLocationByGLN
  , searchLocation
  , uxLocation
  , uxLocationByGLN
  -- * Authenticated API
  , addUser
  , addBusiness
  , addPublicKey
  , revokePublicKey
  , addLocation
  , getBusinessInfo
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

health           :: ClientM HealthResponse
getPublicKey     :: BRKeyId -> ClientM JWK
getPublicKeyInfo :: BRKeyId -> ClientM KeyInfoResponse
searchBusinesses :: Maybe GS1CompanyPrefix -> Maybe Text -> Maybe UTCTime -> ClientM [BusinessResponse]
getLocationByGLN :: LocationEPC -> ClientM LocationResponse
searchLocation   :: Maybe GS1CompanyPrefix -> Maybe UTCTime -> ClientM [LocationResponse]
uxLocation       :: [GS1CompanyPrefix] -> ClientM [BusinessAndLocationResponse]
uxLocationByGLN  :: LocationEPC -> GS1CompanyPrefix -> ClientM BusinessAndLocationResponse
versionInfo      :: ClientM String

addUser          :: BasicAuthData -> NewUser     -> ClientM UserId
addBusiness      :: BasicAuthData -> NewBusiness -> ClientM GS1CompanyPrefix
addPublicKey     :: BasicAuthData -> JWK -> Maybe ExpirationTime -> ClientM BRKeyId
revokePublicKey  :: BasicAuthData -> BRKeyId -> ClientM RevocationTime
addLocation      :: BasicAuthData -> NewLocation -> ClientM LocationId
getBusinessInfo  :: BasicAuthData -> ClientM [BusinessResponse]

_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         health
    :<|> versionInfo
    :<|> getPublicKey
    :<|> getPublicKeyInfo
    :<|> searchBusinesses
    :<|> getLocationByGLN
    :<|> searchLocation
    :<|> uxLocation
    :<|> uxLocationByGLN
  )
  :<|>
  _privAPI@(
         addUser
    :<|> addBusiness
    :<|> addPublicKey
    :<|> revokePublicKey
    :<|> addLocation
    :<|> getBusinessInfo
  )
 ) = client (Proxy :: Proxy ServerAPI)
