module Mirza.OrgRegistry.Client.Servant
  (
  -- * Public API
    health
  , versionInfo
  , getPublicKeyInfo
  , getPublicKey
  , searchOrgs
  , getLocationByGLN
  , searchLocation
  , searchOrgLocation
  , searchOrgLocationByGLN
  -- * Authenticated API
  , addUser
  , addOrg
  , addUserToOrg
  , addPublicKey
  , revokePublicKey
  , addLocation
  , getOrgInfo
  -- * Other
  , authDataToTokenTodoRemove
  ) where

import           Mirza.OrgRegistry.API
import           Mirza.OrgRegistry.Database.Schema (LocationId)
import           Mirza.OrgRegistry.Types           as ORT
import           Mirza.Common.Time                      (ExpirationTime,
                                                         RevocationTime)
import           Mirza.Common.Types                     (ORKeyId)

import           Data.GS1.EPC                           as EPC

import           Crypto.JOSE.JWK                        (JWK)

import           Servant.API
import           Servant.Client
import           Servant.Auth.Client

import           Data.Proxy                             (Proxy (..))
import           Data.Text                              (Text)
import           Data.Time                              (UTCTime)

health                :: ClientM HealthResponse
versionInfo           :: ClientM String
getPublicKeyInfo      :: ORKeyId -> ClientM KeyInfoResponse
getPublicKey          :: ORKeyId -> ClientM JWK
searchOrgs            :: Maybe GS1CompanyPrefix -> Maybe Text -> Maybe UTCTime -> ClientM [OrgResponse]
getLocationByGLN      :: LocationEPC -> ClientM LocationResponse
searchLocation        :: Maybe GS1CompanyPrefix -> Maybe UTCTime -> ClientM [LocationResponse]
searchOrgLocation      :: [GS1CompanyPrefix] -> ClientM [OrgAndLocationResponse]
searchOrgLocationByGLN :: LocationEPC -> GS1CompanyPrefix -> ClientM OrgAndLocationResponse

addUser                :: Token -> ClientM NoContent
addOrg                 :: Token -> GS1CompanyPrefix -> PartialNewOrg -> ClientM NoContent
addUserToOrg           :: Token -> GS1CompanyPrefix -> UserId -> ClientM NoContent
addPublicKey           :: Token -> JWK -> Maybe ExpirationTime -> ClientM ORKeyId
revokePublicKey        :: Token -> ORKeyId -> ClientM RevocationTime
addLocation            :: Token -> NewLocation -> ClientM LocationId
getOrgInfo             :: Token -> ClientM [OrgResponse]

_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         health
    :<|> versionInfo
    :<|> getPublicKeyInfo
    :<|> getPublicKey
    :<|> searchOrgs
    :<|> getLocationByGLN
    :<|> searchLocation
    :<|> searchOrgLocation
    :<|> searchOrgLocationByGLN
  )
  :<|>
  _privAPI@(
         addUser
    :<|> getOrgInfo
    :<|> addOrg
    :<|> addUserToOrg
    :<|> addPublicKey
    :<|> revokePublicKey
    :<|> addLocation
  )
 ) = client (Proxy :: Proxy ServerAPI)

-- This is a filler function which we can use while porting the implementation.
-- There is no implementation for this function because we still need to decide
-- how we are going to auth from the tests.
authDataToTokenTodoRemove :: BasicAuthData -> Token
authDataToTokenTodoRemove = undefined