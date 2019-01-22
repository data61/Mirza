{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.UXUtils
  ( 
    listEventsPretty
  ) where


import           Mirza.Common.GS1BeamOrphans       (LabelEPCUrn (..))
import           Mirza.SupplyChain.EventUtils      (findLabelId,
                                                    getEventList)

import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.ErrorUtils      (throwParseError)
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.Types           hiding (NewUser (..),
                                                    User (..))
import qualified Mirza.SupplyChain.Types           as ST

import qualified Mirza.BusinessRegistry.Types      as BT

import           Data.GS1.DWhat                    (LabelEPC (..), urn2LabelEPC)
import qualified Data.GS1.Event                    as Ev

data PrettyEventResponse =
  PrettyEvent
  { prettyEvent    :: Event
  , prettyLocation :: BT.BusinessAndLocationResponse
  }

-- This takes an EPC urn,
-- and looks up all the events related to that item.
listEventsPretty  :: (Member context '[HasDB],
                      Member err     '[AsServiceError, AsSqlError])
                  => ST.User
                  -> LabelEPCUrn
                  -> AppM context err [Ev.Event]
listEventsPretty _user =
  either
    throwParseError
    (runDb . listEventsPrettyQuery) . urn2LabelEPC . getLabelEPCUrn

listEventsPrettyQuery :: Member err '[AsServiceError]
                      => LabelEPC
                      -> DB context err [Ev.Event]
listEventsPrettyQuery labelEpc = do
  labelIds <- findLabelId labelEpc
  allEvents <- traverse (getEventList . Schema.LabelId) labelIds
  pure $ concat allEvents
