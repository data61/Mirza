module Mirza.Client.Servant
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
  ) where

import           API
import           Model

import           Servant.API
import           Servant.Client

import           Data.Proxy       (Proxy (..))

import           Data.GS1.Event
import           Data.GS1.EventID

import           Data.UUID.Types


newUser      :: NewUser -> ClientM UserID
getKey       :: KeyID -> ClientM RSAPublicKey
getKeyInfo   :: KeyID -> ClientM KeyInfo
businessList :: ClientM [Business]

epcState            :: BasicAuthData -> LabelEPCUrn -> ClientM EPCState
listEvents          :: BasicAuthData -> LabelEPCUrn -> ClientM [Event]
eventInfo           :: BasicAuthData -> EventID -> ClientM (Maybe Event)
contactsInfo        :: BasicAuthData -> ClientM [User]
addContact          :: BasicAuthData -> UserID -> ClientM Bool
removeContact       :: BasicAuthData -> UserID -> ClientM Bool
userSearch          :: BasicAuthData -> String -> ClientM [User]
eventList           :: BasicAuthData -> UserID -> ClientM [Event]
eventUserList       :: BasicAuthData -> EventID -> ClientM [(User, Bool)]
eventSign           :: BasicAuthData -> SignedEvent -> ClientM UUID
eventHashed         :: BasicAuthData -> EventID -> ClientM HashedEvent
insertObjectEvent   :: BasicAuthData -> ObjectEvent -> ClientM Event
insertAggEvent      :: BasicAuthData -> AggregationEvent -> ClientM Event
insertTransactEvent :: BasicAuthData -> TransactionEvent -> ClientM Event
insertTransfEvent   :: BasicAuthData -> TransformationEvent -> ClientM Event
addPublicKey        :: BasicAuthData -> RSAPublicKey -> ClientM KeyID

_api     :: Client ClientM ServerAPI
_privAPI :: Client ClientM ProtectedAPI
_pubAPI  :: Client ClientM PublicAPI
_api@(
  _privAPI@(
    epcState
    :<|> listEvents
    :<|> eventInfo
    :<|> contactsInfo
    :<|> addContact
    :<|> removeContact
    :<|> userSearch
    :<|> eventList
    :<|> eventUserList
    :<|> eventSign
    :<|> eventHashed
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
