
module Mirza.SupplyChain.Client.Servant
  (
  -- * Public API
    health
  , addUser
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
  ) where

import           Mirza.Common.GS1BeamOrphans       (LabelEPCUrn (..))
import           Mirza.SupplyChain.API
import qualified Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.Types           as T

import           Servant.API
import           Servant.Client

import           Data.Proxy                        (Proxy (..))

import           Data.GS1.EPC                      as EPC
import qualified Data.GS1.Event                    as Ev
import           Data.GS1.EventId

import           Data.Text                         (Text)

-- * Public API
health       :: ClientM HealthResponse
addUser      :: NewUser -> ClientM UserId


-- * Authenticated API
contactsInfo        :: BasicAuthData -> ClientM [T.User]
addContact          :: BasicAuthData -> UserId -> ClientM Bool
removeContact       :: BasicAuthData -> UserId -> ClientM Bool
userSearch          :: BasicAuthData -> Maybe GS1CompanyPrefix -> Maybe Text -> ClientM [T.User]

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


_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
         health
    :<|> addUser
  )
  :<|>
  _privAPI@(
         contactsInfo
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
  )
 ) = client (Proxy :: Proxy ServerAPI)
