
module Mirza.Blockchain.Client.Servant
  (
  -- * Public API
    health
  , addEvent
  , getEvents
  , checkEvent
  , getRegisteredEventHash
  ) where

import           Mirza.Blockchain.API
import           Mirza.Blockchain.Types as T

import           Servant.API
import           Servant.Client

import           Data.Proxy             (Proxy (..))

health                 :: ClientM HealthResponse
addEvent               :: EventHash -> IdentifierHash -> ClientM EventInsertionResponse
getEvents              :: IdentifierHash -> ClientM [EventInsertionResponse]
checkEvent             :: EventHash -> ClientM EventInsertionResponse
getRegisteredEventHash :: BlockchainTransactionHash -> ClientM EventInsertionResponse

_api     :: Client ClientM ServerAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         addEvent
    :<|> getEvents
    :<|> checkEvent
    :<|> getRegisteredEventHash
    :<|> health
  )) = client (Proxy :: Proxy ServerAPI)
