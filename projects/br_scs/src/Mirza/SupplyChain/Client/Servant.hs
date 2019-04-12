
module Mirza.SupplyChain.Client.Servant
  (
  -- * Public API
    health
  , addUser
  , versionInfo
  -- * Authenticated API
  , contactsInfo
  , addContact
  , removeContact
  , userSearch
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

import           Data.GS1.EPC                       as EPC
import qualified Data.GS1.Event                     as Ev
import           Data.GS1.EventId

import           Data.Text                          (Text)

-- * Public API
health       :: ClientM HealthResponse
addUser      :: NewUser -> ClientM UserId
versionInfo  :: ClientM String

-- * Authenticated API
contactsInfo        :: ClientM [T.User]
addContact          :: UserId -> ClientM Bool
removeContact       :: UserId -> ClientM Bool
userSearch          :: Maybe GS1CompanyPrefix -> Maybe Text -> ClientM [T.User]

eventSign           :: SignedEvent -> ClientM EventInfo
eventHashed         :: EventId -> ClientM HashedEvent

listEvents          :: LabelEPCUrn -> ClientM [Ev.Event]
eventInfo           :: EventId -> ClientM EventInfo
eventList           :: UserId -> ClientM [Ev.Event]
eventUserList       :: EventId -> ClientM [(T.User, Bool)]
queryUserId         :: ClientM UserId

insertObjectEvent   :: ObjectEvent -> ClientM (EventInfo, Schema.EventId)
insertAggEvent      :: AggregationEvent -> ClientM (EventInfo, Schema.EventId)
insertTransactEvent :: TransactionEvent -> ClientM (EventInfo, Schema.EventId)
insertTransfEvent   :: TransformationEvent -> ClientM (EventInfo, Schema.EventId)

listEventsPretty    :: LabelEPCUrn -> ClientM [PrettyEventResponse]

_api     :: Client ClientM ServerAPI
_api@(   health
    :<|> addUser
    :<|> versionInfo

    :<|> contactsInfo
    :<|> addContact
    :<|> removeContact
    :<|> userSearch

    :<|> eventSign
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

    :<|> listEventsPretty
  ) = client (Proxy :: Proxy ServerAPI)
