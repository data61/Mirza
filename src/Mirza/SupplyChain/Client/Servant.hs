module Mirza.SupplyChain.Client.Servant
  (
  -- * Public API
    newUser
  -- * Authenticated API
  , contactsInfo
  , addContact
  , removeContact
  , userSearch
  , addUserToEvent
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

import           Mirza.SupplyChain.API
import qualified Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.Types           as T

import           Servant.API
import           Servant.Client

import           Data.Proxy                        (Proxy (..))

import qualified Data.GS1.Event                    as Ev
import           Data.GS1.EventId
import           Data.GS1.EPC                      as EPC

import           Data.UUID.Types



-- * Public API
newUser      :: NewUser -> ClientM UserId


-- * Authenticated API
contactsInfo        :: BasicAuthData -> ClientM [T.User]
addContact          :: BasicAuthData -> UserId -> ClientM Bool
removeContact       :: BasicAuthData -> UserId -> ClientM Bool
userSearch          :: BasicAuthData -> UserSearch -> ClientM [T.User]

addUserToEvent      :: BasicAuthData -> UserId -> EventId -> ClientM ()
eventSign           :: BasicAuthData -> SignedEvent -> ClientM UUID
eventHashed         :: BasicAuthData -> EventId -> ClientM HashedEvent

listEvents          :: BasicAuthData -> LabelEPCUrn -> ClientM [Ev.Event]
eventInfo           :: BasicAuthData -> EventId -> ClientM (Maybe Ev.Event)
eventList           :: BasicAuthData -> UserId -> ClientM [Ev.Event]
eventUserList       :: BasicAuthData -> EventId -> ClientM [(T.User, Bool)]
queryUserId         :: BasicAuthData -> ClientM UserId

insertObjectEvent   :: BasicAuthData -> ObjectEvent -> ClientM (Ev.Event, Schema.EventId)
insertAggEvent      :: BasicAuthData -> AggregationEvent -> ClientM (Ev.Event, Schema.EventId)
insertTransactEvent :: BasicAuthData -> TransactionEvent -> ClientM (Ev.Event, Schema.EventId)
insertTransfEvent   :: BasicAuthData -> TransformationEvent -> ClientM (Ev.Event, Schema.EventId)


_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _pubAPI@(
    newUser
  )
  :<|>
  _privAPI@(
         contactsInfo
    :<|> addContact
    :<|> removeContact
    :<|> userSearch

    :<|> addUserToEvent
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
