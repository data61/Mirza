module Mirza.Trails.Client.Servant
  (
  -- * Public API
    health
  , versionInfo
  ) where


import           Mirza.Trails.API

import           Mirza.Common.Types

import           Data.Proxy         (Proxy (..))
import           Servant.API
import           Servant.Client


health                :: ClientM HealthResponse
versionInfo           :: ClientM String


_api     :: Client ClientM ServerAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         health
    :<|> versionInfo
  )
 ) = client (Proxy :: Proxy ServerAPI)
