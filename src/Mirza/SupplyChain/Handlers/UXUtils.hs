{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Mirza.SupplyChain.Handlers.UXUtils
  (
    listEventsPretty
  , PrettyEventResponse (..)
  ) where


import           GHC.Generics                          (Generic)

import           Mirza.Common.GS1BeamOrphans           (LabelEPCUrn (..))

import           Mirza.SupplyChain.Handlers.Queries    (listEventsQuery)

import           Mirza.SupplyChain.ErrorUtils          (throwParseError)
import           Mirza.SupplyChain.Types               hiding (NewUser (..),
                                                        User (..))
import qualified Mirza.SupplyChain.Types               as ST

import qualified Mirza.BusinessRegistry.Types          as BT

import           Data.GS1.DWhat                        (getCompanyPrefix,
                                                        urn2LabelEPC)
import qualified Data.GS1.Event                        as Ev

import           Mirza.BusinessRegistry.Client.Servant (uxLocation)

import           Data.Aeson
import           Data.Aeson.TH

import           Data.Swagger                          (ToSchema (..))

data PrettyEventResponse =
  PrettyEvent
  { prettyEvent    :: [Ev.Event]
  , prettyLocation :: [BT.BusinessAndLocationResponse]
  } deriving (Show, Eq, Generic)
$(deriveJSON defaultOptions ''PrettyEventResponse)
instance ToSchema PrettyEventResponse

-- This takes an EPC urn,
-- and looks up all the events related to that item.
listEventsPretty  :: (Member context '[HasDB, HasBRClientEnv],
                      Member err     '[AsServiceError, AsServantError, AsSqlError])
                  => ST.User
                  -> LabelEPCUrn
                  -> AppM context err PrettyEventResponse
listEventsPretty _user lblUrn = do
  case urn2LabelEPC . getLabelEPCUrn $ lblUrn of
    Left err -> throwParseError err
    Right lbl -> do
      let pfx = getCompanyPrefix lbl
      bizLocResp <- runClientFunc $ uxLocation [pfx]
      evs <- runDb $ listEventsQuery lbl
      pure $ PrettyEvent evs bizLocResp
