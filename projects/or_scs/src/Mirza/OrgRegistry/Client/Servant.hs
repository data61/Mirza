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
  ) where

import           Mirza.Common.Time                 (ExpirationTime,
                                                    RevocationTime)
import           Mirza.Common.Types                (ORKeyId)
import           Mirza.OrgRegistry.API
import           Mirza.OrgRegistry.Database.Schema (LocationPrimaryKey)
import           Mirza.OrgRegistry.Types           as ORT

import           Data.GS1.EPC                      as EPC

import           Crypto.JOSE.JWK                   (JWK)

import           Servant.API
import           Servant.Auth.Client
import           Servant.Client

import           Data.Proxy                        (Proxy (..))
import           Data.Text                         (Text)
import           Data.Time                         (UTCTime)

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
addUserToOrg           :: Token -> GS1CompanyPrefix -> OAuthSub -> ClientM NoContent
addPublicKey           :: Token -> GS1CompanyPrefix -> JWK -> Maybe ExpirationTime -> ClientM ORKeyId
revokePublicKey        :: Token -> ORKeyId -> ClientM RevocationTime
addLocation            :: Token -> NewLocation -> ClientM LocationPrimaryKey
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
