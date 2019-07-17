module Mirza.Trails.Client.Servant
  (
  -- * Public API
    health
  , versionInfo
  , getTrail
  , addTrail
  ) where


import           Mirza.Trails.API
import           Mirza.Trails.Types

import           Mirza.Common.GS1Orphans ()
import           Mirza.Common.Types

import           Data.GS1.EventId        (EventId)

import           Data.Proxy              (Proxy (..))
import           Servant.API
import           Servant.Client


health                :: ClientM HealthResponse
versionInfo           :: ClientM String
getTrail              :: EventId -> ClientM [TrailEntryResponse]
addTrail              :: TrailEntryResponse -> ClientM NoContent


_api     :: Client ClientM ServerAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         health
    :<|> versionInfo
    :<|> getTrail
    :<|> addTrail
  )
 ) = client (Proxy :: Proxy ServerAPI)
