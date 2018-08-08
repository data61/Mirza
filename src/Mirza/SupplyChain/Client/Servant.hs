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
  , epcState
  , listEvents
  , eventInfo
  , eventList
  , eventUserList
  , insertObjectEvent
  , insertAggEvent
  , insertTransactEvent
  , insertTransfEvent
  ) where

import           Mirza.SupplyChain.API
import qualified Mirza.SupplyChain.StorageBeam as SB
import           Mirza.SupplyChain.Types       as T

import           Servant.API
import           Servant.Client

import           Data.Proxy                    (Proxy (..))

import qualified Data.GS1.Event                as Ev
import           Data.GS1.EventId

import           Data.UUID.Types



-- * Public API
newUser      :: NewUser -> ClientM UserId


-- * Authenticated API
contactsInfo        :: BasicAuthData -> ClientM [T.User]
addContact          :: BasicAuthData -> UserId -> ClientM Bool
removeContact       :: BasicAuthData -> UserId -> ClientM Bool
userSearch          :: BasicAuthData -> String -> ClientM [T.User]

addUserToEvent      :: BasicAuthData -> UserId -> EventId -> ClientM ()
eventSign           :: BasicAuthData -> SignedEvent -> ClientM UUID
eventHashed         :: BasicAuthData -> EventId -> ClientM HashedEvent

epcState            :: BasicAuthData -> LabelEPCUrn -> ClientM EPCState
listEvents          :: BasicAuthData -> LabelEPCUrn -> ClientM [Ev.Event]
eventInfo           :: BasicAuthData -> EventId -> ClientM (Maybe Ev.Event)
eventList           :: BasicAuthData -> UserId -> ClientM [Ev.Event]
eventUserList       :: BasicAuthData -> EventId -> ClientM [(T.User, Bool)]

insertObjectEvent   :: BasicAuthData -> ObjectEvent -> ClientM (Ev.Event, SB.EventId)
insertAggEvent      :: BasicAuthData -> AggregationEvent -> ClientM (Ev.Event, SB.EventId)
insertTransactEvent :: BasicAuthData -> TransactionEvent -> ClientM (Ev.Event, SB.EventId)
insertTransfEvent   :: BasicAuthData -> TransformationEvent -> ClientM (Ev.Event, SB.EventId)


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

    :<|> epcState
    :<|> listEvents
    :<|> eventInfo
    :<|> eventList
    :<|> eventUserList

    :<|> insertObjectEvent
    :<|> insertAggEvent
    :<|> insertTransactEvent
    :<|> insertTransfEvent
  )
 ) = client (Proxy :: Proxy ServerAPI)
