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
  , encodeEvent, decodeEvent
  , eventTxtToBS
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

import           Control.Monad.Except              (MonadError, catchError)


-- | Ueful for handling specific errors from, for example, database transactions
-- @
--  handleError errHandler $ runDb ...
--  ...
--  where errHandler (AppError (DatabaseError sqlErr)) = ...
--        errHandler e = throwError e
-- @
handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

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
storageToModelEvent = decodeEvent . Schema.event_json

-- | Converts a DB representation of ``User`` to a Model representation
-- Schema.User = Schema.User uid bizId fName lName phNum passHash email
userTableToModel :: Schema.User -> ST.User
userTableToModel (Schema.User uid _ fName lName _ _ _)
    = ST.User (ST.UserId uid) fName lName


encodeEvent :: Ev.Event -> ByteString
encodeEvent = toStrict . encode

-- XXX is this the right encoding to use? It's used for checking signatures
-- and hashing the json.
eventTxtToBS :: T.Text -> ByteString
eventTxtToBS = En.encodeUtf8

decodeEvent :: ByteString -> Maybe Ev.Event
decodeEvent = decodeStrict
