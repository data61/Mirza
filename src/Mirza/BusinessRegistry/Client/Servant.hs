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
import           Mirza.BusinessRegistry.Types  as BRT
import           Mirza.Common.Types            (KeyID)
import qualified Mirza.SupplyChain.StorageBeam as SB

import           Servant.API
import           Servant.Client

import           Data.Proxy                    (Proxy (..))

import qualified Data.GS1.Event                as Ev
import           Data.GS1.EventId

import           Data.Time.Clock               (UTCTime)
import           Data.UUID.Types

getKey       :: KeyID -> ClientM PEM_RSAPubKey
getKeyInfo   :: KeyID -> ClientM KeyInfoResponse
businessList :: ClientM [BusinessResponse]

addPublicKey        :: BasicAuthData -> PEM_RSAPubKey -> Maybe ExpirationTime -> ClientM KeyID
revokePublicKey     :: BasicAuthData -> KeyID -> ClientM UTCTime

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
