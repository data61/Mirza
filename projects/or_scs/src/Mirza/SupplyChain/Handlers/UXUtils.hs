{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Mirza.SupplyChain.Handlers.UXUtils
  ( listEventsPretty
  , PrettyEventResponse (..)
  ) where

import           GHC.Generics                       (Generic)

import           GHC.Generics                       (Generic)

import           Mirza.Common.GS1BeamOrphans        (LabelEPCUrn (..))

import           Mirza.SupplyChain.Handlers.Queries (listEventsQuery)

import           Mirza.SupplyChain.ErrorUtils       (throwParseError)
import           Mirza.SupplyChain.EventUtils       (getParent)
import           Mirza.SupplyChain.Types

import           Mirza.OrgRegistry.Types            (OrgAndLocationResponse (..),
                                                     OrgResponse (..))
import qualified Mirza.OrgRegistry.Types            as ORT

import           Data.GS1.DWhat
import           Data.GS1.DWhen                     (DWhen (..))
import           Data.GS1.DWhere
import           Data.GS1.EPC
import qualified Data.GS1.Event                     as Ev

import           Mirza.OrgRegistry.Client.Servant   (searchOrgLocationByGLN)

import           Data.Aeson

import           Data.Swagger                       (ToSchema (..))

import           Data.Either                        (either)
import           Data.List                          (nub, sortOn)

data PrettyEventResponse =
  PrettyEventResponse
  { prettyEvent    :: Ev.Event
  , prettyLocation :: Maybe OrgAndLocationResponse
  } deriving (Show, Eq, Generic)

instance FromJSON PrettyEventResponse where
  parseJSON = withObject "PrettyEventResponse" $ \v -> PrettyEventResponse
    <$> v .: "event"
    <*> v .: "orglocation"

instance ToJSON PrettyEventResponse where
  toJSON (PrettyEventResponse ev (Just orgLoc)) = object
    [ "eventType" .= Ev._etype ev
    , "orgName" .= (orgResponseName . orgResponse) orgLoc
    , "orgLocation" .= locationResponse orgLoc
    ]
  toJSON (PrettyEventResponse ev Nothing) = object
    [ "eventType" .= Ev._etype ev
    ]

instance ToSchema PrettyEventResponse

-- This takes an EPC urn,
-- and looks up all the events related to that item.
listEventsPretty  :: (Member context '[HasDB, HasORClientEnv],
                      Member err     '[AsServiceError, AsServantError, AsSqlError, ORT.AsORError])
                  => LabelEPCUrn
                  -> AppM context err [PrettyEventResponse]
listEventsPretty = either throwParseError fetchPrettyEvents . urn2LabelEPC . getLabelEPCUrn

fetchPrettyEvents :: (Member context '[HasDB, HasORClientEnv],
                    Member err     '[AsServiceError, AsServantError, AsSqlError, ORT.AsORError])
                  => LabelEPC
                  -> AppM context err [PrettyEventResponse]
fetchPrettyEvents lbl = do
  evs <- runDb $ listEventsQuery lbl
  currEvs <- traverse eventToPrettyEvent evs
  (pEvs :: [PrettyEventResponse] ) <- concat <$> traverse getParentEvents evs
  let allEvs = currEvs <> pEvs
  pure $ sortOn ( _eventTime . Ev._when . prettyEvent) $ nub allEvs


getParentEvents :: (Member context '[HasDB, HasORClientEnv],
                    Member err     '[AsServiceError, AsServantError, AsSqlError, ORT.AsORError])
                => Ev.Event
                -> AppM context err [PrettyEventResponse]
getParentEvents ev = do
  let mParentLabel = getParent . Ev._what $ ev
  case mParentLabel of
    Nothing              -> pure []
    Just (ParentLabel p) -> fetchPrettyEvents (IL p)


eventToPrettyEvent :: (Member context '[HasDB, HasORClientEnv],
                       Member err     '[AsServiceError, AsServantError, AsSqlError, ORT.AsORError])
                   => Ev.Event
                   -> AppM context err PrettyEventResponse
eventToPrettyEvent ev = do
  let mloc = _readPoint . Ev._where $ ev
  case mloc of
    Nothing -> pure $ PrettyEventResponse ev Nothing
    Just (ReadPointLocation loc) -> do
      let pfx = _sglnCompanyPrefix loc
      locResp <- runClientFunc $ Just <$> searchOrgLocationByGLN loc pfx
      pure $ PrettyEventResponse ev locResp
