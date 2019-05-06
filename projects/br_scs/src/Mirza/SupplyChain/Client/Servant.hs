module Mirza.SupplyChain.Client.Servant
  (
  -- * Public API
    health
  , addUser
  , versionInfo
  -- * Authenticated API
  , eventSign
  , eventHashed
  , listEvents
  , eventInfo
  , eventList
  , eventUserList
  , queryUserId
  , insertObjectEvent
  , insertAggEvent
  , insertTransactEvent
  , insertTransfEvent
  , listEventsPretty
  ) where

import           Mirza.Common.GS1BeamOrphans        (LabelEPCUrn (..))
import           Mirza.SupplyChain.API
import qualified Mirza.SupplyChain.Database.Schema  as Schema
import           Mirza.SupplyChain.Handlers.UXUtils (PrettyEventResponse (..))
import           Mirza.SupplyChain.Types            as T


import           Servant.API
import           Servant.Client

import           Data.Proxy                         (Proxy (..))

import qualified Data.GS1.Event                     as Ev
import           Data.GS1.EventId

-- * Public API
health       :: ClientM HealthResponse
addUser      :: NewUser -> ClientM UserId
versionInfo  :: ClientM String

-- * Authenticated API
eventSign           :: BasicAuthData -> SignedEvent -> ClientM EventInfo
eventHashed         :: BasicAuthData -> EventId -> ClientM HashedEvent

listEvents          :: BasicAuthData -> LabelEPCUrn -> ClientM [Ev.Event]
eventInfo           :: BasicAuthData -> EventId -> ClientM EventInfo
eventList           :: BasicAuthData -> UserId -> ClientM [Ev.Event]
eventUserList       :: BasicAuthData -> EventId -> ClientM [(T.User, Bool)]
queryUserId         :: BasicAuthData -> ClientM UserId

insertObjectEvent   :: BasicAuthData -> ObjectEvent -> ClientM (EventInfo, Schema.EventId)
insertAggEvent      :: BasicAuthData -> AggregationEvent -> ClientM (EventInfo, Schema.EventId)
insertTransactEvent :: BasicAuthData -> TransactionEvent -> ClientM (EventInfo, Schema.EventId)
insertTransfEvent   :: BasicAuthData -> TransformationEvent -> ClientM (EventInfo, Schema.EventId)

listEventsPretty    :: BasicAuthData -> LabelEPCUrn -> ClientM [PrettyEventResponse]

_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_frontEndAPI :: Client ClientM UIAPI
_api@(
  _pubAPI@(
         health
    :<|> addUser
    :<|> versionInfo
  )
  :<|>
  _privAPI@(
         eventSign
    :<|> eventHashed

    :<|> listEvents
    :<|> eventInfo
    :<|> eventList
    :<|> eventUserList
    :<|> queryUserId

    :<|> insertObjectEvent
    :<|> insertAggEvent
    :<|> insertTransactEvent
    :<|> insertTransfEvent
  )
  :<|>
  _frontEndAPI@(
    listEventsPretty
  )
 ) = client (Proxy :: Proxy ServerAPI)
