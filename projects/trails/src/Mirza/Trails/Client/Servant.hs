module Mirza.Trails.Client.Servant
  (
  -- * Public API
    health
  , versionInfo
  , getTrailByEventId
  , getTrailBySignature
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
getTrailByEventId     :: EventId -> ClientM [TrailEntry]
getTrailBySignature   :: SignaturePlaceholder -> ClientM [TrailEntry]
addTrail              :: [TrailEntry] -> ClientM NoContent


_api     :: Client ClientM ServerAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         health
    :<|> versionInfo
    :<|> getTrailByEventId
    :<|> getTrailBySignature
    :<|> addTrail
  )
 ) = client (Proxy :: Proxy ServerAPI)
