
module Mirza.Blockchain.Client.Servant
  (
  -- * Public API
    health
  , addEvent
  , getEvents
  , checkEvent
  , getEventHash
  ) where

import           Mirza.Blockchain.API
import           Mirza.Blockchain.Types as T

import           Servant.API
import           Servant.Client

import           Data.Proxy             (Proxy (..))

-- * Public API
health :: ClientM HealthResponse

addEvent :: EventHash -> IdentifierHash -> ClientM EventInsertionResponse
getEvents :: IdentifierHash -> ClientM [EventInsertionResponse]
checkEvent :: EventHash -> ClientM EventInsertionResponse
getEventHash :: BlockchainTransactionHash -> ClientM EventInsertionResponse

_api     :: Client ClientM ServerAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         addEvent
    :<|> getEvents
    :<|> checkEvent
    :<|> getEventHash
    :<|> health
  )
 ) = client (Proxy :: Proxy ServerAPI)
