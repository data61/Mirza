module Mirza.SupplyChain.Client.Servant
  ( health
  , versionInfo
  , eventSign
  , listEvents
  , eventInfo
  , insertGS1Event
  , listEventsPretty
  ) where

import           Mirza.Common.GS1BeamOrphans        (LabelEPCUrn (..))
import           Mirza.Common.GS1Orphans            ()
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
health      :: ClientM HealthResponse
versionInfo :: ClientM String

-- * Authenticated API

eventSign        :: SignedEvent -> ClientM EventInfo
listEvents       :: LabelEPCUrn -> ClientM [Ev.Event]
eventInfo        :: EventId -> ClientM EventInfo
insertGS1Event   :: Ev.Event -> ClientM (EventInfo, Schema.EventId)
listEventsPretty :: LabelEPCUrn -> ClientM [PrettyEventResponse]

_api     :: Client ClientM ServerAPI
_api@(   health
    :<|> versionInfo

    :<|> eventSign

    :<|> listEvents
    :<|> eventInfo

    :<|> insertGS1Event

    :<|> listEventsPretty
  ) = client (Proxy :: Proxy ServerAPI)
