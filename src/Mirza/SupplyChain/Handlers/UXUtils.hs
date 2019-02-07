{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import           Data.GS1.DWhere
import           Data.GS1.EPC
import qualified Data.GS1.Event                        as Ev

import           Mirza.BusinessRegistry.Client.Servant (uxLocationByGLN)

import           Data.Aeson

import           Data.Swagger                          (ToSchema (..))

data PrettyEventResponse =
  PrettyEventResponse
  { prettyEvent    :: Ev.Event
  , prettyLocation :: Maybe BT.BusinessAndLocationResponse
  } deriving (Show, Eq, Generic)

instance FromJSON PrettyEventResponse where
  parseJSON = withObject "PrettyEventResponse" $ \v -> PrettyEventResponse
    <$> v .: "event"
    <*> v .: "businesslocation"

instance ToJSON PrettyEventResponse where
  toJSON (PrettyEventResponse ev loc) = error "lol"

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
    Left err -> throwParseError err
    Right lbl -> do
      let pfx = getCompanyPrefix lbl
      evs <- runDb $ listEventsQuery lbl
      traverse func evs


func :: (Member context '[HasDB, HasBRClientEnv],
         Member err     '[AsServiceError, AsServantError, AsSqlError, BT.AsBRError])
      => Ev.Event -> AppM context err PrettyEventResponse
func ev = do
  let mloc = _readPoint . Ev._where $ ev
  case mloc of
    Nothing -> throwing BT._LocationNotKnownBRE ()
    Just (ReadPointLocation loc) -> do
      let pfx = _sglnCompanyPrefix loc
      locResp <- runClientFunc $ Just <$> uxLocationByGLN loc pfx
      pure $ PrettyEventResponse ev locResp
