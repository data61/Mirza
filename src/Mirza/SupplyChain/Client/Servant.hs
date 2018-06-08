module Mirza.SupplyChain.Client.Servant
  (
  -- * Public API
   newUser
  ,getKey
  ,getKeyInfo
  ,businessList
  -- * Authenticated API
  ,epcState
  ,listEvents
  ,eventInfo
  ,contactsInfo
  ,addContact
  ,removeContact
  ,userSearch
  ,eventList
  ,eventUserList
  ,eventSign
  ,eventHashed
  ,insertObjectEvent
  ,insertAggEvent
  ,insertTransactEvent
  ,insertTransfEvent
  ,addPublicKey
  ,addUserToEvent
  ) where

import           Mirza.SupplyChain.API
import           Mirza.SupplyChain.Types

import           Servant.API
import           Servant.Client

import           Data.Proxy              (Proxy (..))

import           Data.GS1.Event
import           Data.GS1.EventId

import           Data.UUID.Types


newUser      :: NewUser -> ClientM UserID
getKey       :: KeyID -> ClientM PEM_RSAPubKey
getKeyInfo   :: KeyID -> ClientM KeyInfo
businessList :: ClientM [Business]

epcState            :: BasicAuthData -> LabelEPCUrn -> ClientM EPCState
listEvents          :: BasicAuthData -> LabelEPCUrn -> ClientM [Event]
eventInfo           :: BasicAuthData -> EventId -> ClientM (Maybe Event)
contactsInfo        :: BasicAuthData -> ClientM [User]
addContact          :: BasicAuthData -> UserID -> ClientM Bool
removeContact       :: BasicAuthData -> UserID -> ClientM Bool
userSearch          :: BasicAuthData -> String -> ClientM [User]
eventList           :: BasicAuthData -> UserID -> ClientM [Event]
eventUserList       :: BasicAuthData -> EventId -> ClientM [(User, Bool)]
eventSign           :: BasicAuthData -> SignedEvent -> ClientM UUID
eventHashed         :: BasicAuthData -> EventId -> ClientM HashedEvent
insertObjectEvent   :: BasicAuthData -> ObjectEvent -> ClientM Event
insertAggEvent      :: BasicAuthData -> AggregationEvent -> ClientM Event
insertTransactEvent :: BasicAuthData -> TransactionEvent -> ClientM Event
insertTransfEvent   :: BasicAuthData -> TransformationEvent -> ClientM Event
addUserToEvent      :: BasicAuthData -> UserID -> EventId -> ClientM ()
addPublicKey        :: BasicAuthData -> PEM_RSAPubKey -> ClientM KeyID

_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
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

    :<|> addPublicKey
  )
  :<|>
  _pubAPI@(
    newUser
    :<|>getKey
    :<|>getKeyInfo
    :<|>businessList
  )
 ) = client (Proxy :: Proxy ServerAPI)
