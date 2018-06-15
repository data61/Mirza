{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

-- | Following are a bunch of utility functions to do household stuff like
-- generating primary keys, timestamps - stuff that almost every function
-- below would need to do anyway
-- functions that start with `insert` does some database operation
-- functions that start with `to` converts between
-- Model type and its Storage equivalent
module Mirza.SupplyChain.QueryUtils
  (
    storageToModelBusiness , storageToModelEvent, userTableToModel
  , generatePk
  , onLocalTime , toLocalTime, toZonedTime, generateTimeStamp
  , encodeEvent, decodeEvent
  , eventTxtToBS
  , handleError
  , withPKey
  ) where

import qualified Mirza.SupplyChain.StorageBeam as SB
import           Mirza.SupplyChain.Types       hiding (Business (..), User (..))
import qualified Mirza.SupplyChain.Types       as ST

import           Data.GS1.EPC                  as EPC
import           Data.GS1.Event                (Event (..))
import qualified Data.GS1.Event                as Ev

import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (decode)
import           Data.Aeson.Text               (encodeToLazyText)
import           Data.ByteString               (ByteString)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as En
import qualified Data.Text.Lazy                as TxtL
import qualified Data.Text.Lazy.Encoding       as LEn
import           Data.Time                     (UTCTime, ZonedTime (..),
                                                utcToZonedTime)
import           Data.Time.Clock               (getCurrentTime)
import           Data.Time.LocalTime           (LocalTime, localTimeToUTC, utc,
                                                utcToLocalTime)
import           Data.UUID.V4                  (nextRandom)
import           Database.Beam                 as B

import           Control.Monad.Except          (MonadError, catchError)


-- | Reads back the ``LocalTime`` in UTCTime (with an offset of 0)
-- And wraps it in a custom constructor (newtype wrappers around UTCTime)
onLocalTime :: (UTCTime -> t) -> LocalTime -> t
onLocalTime c t = c (localTimeToUTC utc t)

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
toLocalTime :: EPCISTime -> LocalTime
toLocalTime = utcToLocalTime utc . unEPCISTime

-- | Shorthand for type-casting UTCTime to LocalTime before storing them in DB
toZonedTime :: UTCTime -> ZonedTime
toZonedTime = utcToZonedTime utc

-- | Generates a timestamp in LocalTime + 0:00 offset
-- which is a UTCTime
generateTimeStamp :: MonadIO m => m LocalTime
generateTimeStamp = utcToLocalTime utc <$> liftIO getCurrentTime


-- | shorthand for wrapping ``UUID.nextRandom`` in ``AppM``
generatePk :: MonadIO m => m SB.PrimaryKeyType
generatePk = liftIO nextRandom

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
withPKey :: MonadIO m => (SB.PrimaryKeyType -> m a) -> m SB.PrimaryKeyType
withPKey f = do
  pKey <- generatePk
  _ <- f pKey
  pure pKey


storageToModelBusiness :: SB.Business -> ST.Business
storageToModelBusiness (SB.Business pfix name f site addr lat long)
  = ST.Business pfix name f site addr lat long

storageToModelEvent :: SB.Event -> Maybe Ev.Event
storageToModelEvent = decodeEvent . SB.json_event

-- | Converts a DB representation of ``User`` to a Model representation
-- SB.User = SB.User uid bizId fName lName phNum passHash email
userTableToModel :: SB.User -> ST.User
userTableToModel (SB.User uid _ fName lName _ _ _) = ST.User (UserID uid) fName lName


encodeEvent :: Event -> T.Text
encodeEvent event = TxtL.toStrict  (encodeToLazyText event)

-- XXX is this the right encoding to use? It's used for checking signatures
-- and hashing the json.
eventTxtToBS :: T.Text -> ByteString
eventTxtToBS = En.encodeUtf8

decodeEvent :: T.Text -> Maybe Ev.Event
decodeEvent = decode . LEn.encodeUtf8 . TxtL.fromStrict

