{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Following are a bunch of utility functions to do household stuff like
-- generating primary keys, timestamps - stuff that almost every function
-- below would need to do anyway
-- functions that start with `insert` does some database operation
-- functions that start with `to` converts between
-- Model type and its Storage equivalent
module Mirza.SupplyChain.QueryUtils
  (
    storageToModelEvent, userTableToModel
  , encodeEventToJSON, decodeEventFromJSON, constructEventToSign
  , handleError
  , withPKey
  ) where

import           Mirza.Common.Utils
import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.Types           hiding (User (..))
import qualified Mirza.SupplyChain.Types           as ST

import qualified Data.GS1.Event                    as Ev

import           Data.Aeson                        (decodeStrict, encode)
import           Data.ByteString                   (ByteString)
import           Data.ByteString.Lazy              (toStrict)
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as En


-- | Handles the common case of generating a primary key, using it in some
-- transaction and then returning the primary key.
-- @
--   insertFoo :: ArgType -> AppM PrimmaryKeyType
--   insertFoo arg = withPKey $ \pKey -> do
--     runDb ... pKey ...
-- @
withPKey :: MonadIO m => (PrimaryKeyType -> m a) -> m PrimaryKeyType
withPKey f = do
  pKey <- newUUID
  _ <- f pKey
  pure pKey


storageToModelEvent :: Schema.Event -> Maybe Ev.Event
storageToModelEvent = decodeEventFromJSON . Schema.event_json

-- | Converts a DB representation of ``User`` to a Model representation
-- Schema.User = Schema.User uid bizId fName lName phNum passHash email
userTableToModel :: Schema.User -> ST.User
userTableToModel (Schema.User uid _ fName lName _ _ _)
    = ST.User (ST.UserId uid) fName lName

-- | This function returns the standardised form of event that users can sign
constructEventToSign :: Ev.Event ->ByteString
constructEventToSign = encodeEventToJSON

encodeEventToJSON :: Ev.Event -> ByteString
encodeEventToJSON = toStrict . encode

decodeEventFromJSON :: ByteString -> Maybe Ev.Event
decodeEventFromJSON = decodeStrict
