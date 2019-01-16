{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.UXUtils
  ( 
    listEventsPretty
  ) where


import           Mirza.Common.GS1BeamOrphans       (LabelEPCUrn (..))
import           Mirza.Common.Utils                (fromPgJSON)
import           Mirza.SupplyChain.EventUtils      (findLabelId,
                                                    findSchemaEvent,
                                                    getEventList)
import           Mirza.SupplyChain.Handlers.Users  (userTableToModel)

import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.ErrorUtils      (throwBackendError,
                                                    throwParseError)
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.Types           hiding (NewUser (..),
                                                    User (..))
import qualified Mirza.SupplyChain.Types           as ST

import           Data.GS1.DWhat                    (LabelEPC (..), urn2LabelEPC)
import qualified Data.GS1.Event                    as Ev
import           Data.GS1.EventId                  as EvId

import           Database.Beam                     as B

import           Control.Lens                      (( # ))
import           Control.Monad.Error.Hoist

import           Data.Bifunctor                    (bimap)

import           Crypto.JOSE.Types                 (Base64Octets (..))

import           Data.List                         (partition)

import           Database.Beam.Postgres            (PgJSON (..))

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
