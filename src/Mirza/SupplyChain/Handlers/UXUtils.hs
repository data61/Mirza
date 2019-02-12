{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Mirza.SupplyChain.Handlers.UXUtils
  (
    listEventsPretty
  , PrettyEventResponse (..)
  ) where


import           GHC.Generics                          (Generic)

import           Mirza.Common.GS1BeamOrphans           (LabelEPCUrn (..))

import           Mirza.SupplyChain.Handlers.Queries    (listEventsQuery)

import           Mirza.SupplyChain.ErrorUtils          (throwParseError)
import           Mirza.SupplyChain.EventUtils          (getParent)
import           Mirza.SupplyChain.Types               hiding (NewUser (..),
                                                        User (..))
import qualified Mirza.SupplyChain.Types               as ST

import           Mirza.BusinessRegistry.Types          (BusinessAndLocationResponse (..),
                                                        BusinessResponse (..))
import qualified Mirza.BusinessRegistry.Types          as BT

import           Data.GS1.DWhat
import           Data.GS1.DWhen                        (DWhen (..))
import           Data.GS1.DWhere
import           Data.GS1.EPC
import qualified Data.GS1.Event                        as Ev

import           Mirza.BusinessRegistry.Client.Servant (uxLocationByGLN)

import           Data.Aeson

import           Data.Swagger                          (ToSchema (..))

import           Data.List                             (sortOn)

data PrettyEventResponse =
  PrettyEventResponse
  { prettyEvent    :: Ev.Event
  , prettyLocation :: Maybe BusinessAndLocationResponse
  } deriving (Show, Eq, Generic)

instance FromJSON PrettyEventResponse where
  parseJSON = withObject "PrettyEventResponse" $ \v -> PrettyEventResponse
    <$> v .: "event"
    <*> v .: "businesslocation"

instance ToJSON PrettyEventResponse where
  toJSON (PrettyEventResponse ev (Just bizLoc)) = object
    [ "eventType" .= Ev._etype ev
    , "businessName" .= (businessName . businessResponse) bizLoc
    , "bizLocation" .= locationResponse bizLoc
    ]
  toJSON (PrettyEventResponse ev Nothing) = object
    [ "eventType" .= Ev._etype ev
    ]

instance ToSchema PrettyEventResponse

-- This takes an EPC urn,
-- and looks up all the events related to that item.
listEventsPretty  :: (Member context '[HasDB, HasBRClientEnv],
                      Member err     '[AsServiceError, AsServantError, AsSqlError, BT.AsBRError])
                  => ST.User
                  -> LabelEPCUrn
                  -> AppM context err [PrettyEventResponse]
listEventsPretty _user lblUrn = do
  case urn2LabelEPC . getLabelEPCUrn $ lblUrn of
    Left err  -> throwParseError err
    Right lbl -> fetchPrettyEvents lbl

fetchPrettyEvents :: (Member context '[HasDB, HasBRClientEnv],
                    Member err     '[AsServiceError, AsServantError, AsSqlError, BT.AsBRError])
                  => LabelEPC
                  -> AppM context err [PrettyEventResponse]
fetchPrettyEvents lbl = do
  evs <- runDb $ listEventsQuery lbl
  currEvs <- traverse eventToPrettyEvent evs
  (pEvs :: [PrettyEventResponse] ) <- concat <$> traverse getParentEvents evs
  let allEvs = currEvs <> pEvs
  pure $ sortOn ( _eventTime . Ev._when . prettyEvent) allEvs


getParentEvents :: (Member context '[HasDB, HasBRClientEnv],
                    Member err     '[AsServiceError, AsServantError, AsSqlError, BT.AsBRError])
                => Ev.Event
                -> AppM context err [PrettyEventResponse]
getParentEvents ev = do
  let mParentLabel = getParent . Ev._what $ ev
  case mParentLabel of
    Nothing              -> pure []
    Just (ParentLabel p) -> fetchPrettyEvents (IL p)


eventToPrettyEvent :: (Member context '[HasDB, HasBRClientEnv],
                       Member err     '[AsServiceError, AsServantError, AsSqlError, BT.AsBRError])
                   => Ev.Event
                   -> AppM context err PrettyEventResponse
eventToPrettyEvent ev = do
  let mloc = _readPoint . Ev._where $ ev
  case mloc of
    Nothing -> throwing BT._LocationNotKnownBRE ()
    Just (ReadPointLocation loc) -> do
      let pfx = _sglnCompanyPrefix loc
      locResp <- runClientFunc $ Just <$> uxLocationByGLN loc pfx
      pure $ PrettyEventResponse ev locResp
