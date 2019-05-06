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
  , addBusiness
  , addUserToBusiness
  , addPublicKey
  , revokePublicKey
  , addLocation
  , getBusinessInfo
  -- * Other
  , authDataToTokenTodoRemove
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
import           Servant.Auth.Client

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

addBusiness      :: Token -> NewBusiness -> ClientM GS1CompanyPrefix
addUserToBusiness :: Token -> GS1CompanyPrefix -> UserId -> ClientM NoContent
addPublicKey     :: Token -> JWK -> Maybe ExpirationTime -> ClientM BRKeyId
revokePublicKey  :: Token -> BRKeyId -> ClientM RevocationTime
addLocation      :: Token -> NewLocation -> ClientM LocationId
getBusinessInfo  :: Token -> ClientM [BusinessResponse]

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
         addBusiness
    :<|> addUserToBusiness
    :<|> addPublicKey
    :<|> revokePublicKey
    :<|> addLocation
    :<|> getBusinessInfo
  )
 ) = client (Proxy :: Proxy ServerAPI)

-- This is a filler function which we can use while porting the implementation.
-- There is no implementation for this function because we still need to decide
-- how we are going to auth from the tests.
authDataToTokenTodoRemove :: BasicAuthData -> Token
authDataToTokenTodoRemove = undefined