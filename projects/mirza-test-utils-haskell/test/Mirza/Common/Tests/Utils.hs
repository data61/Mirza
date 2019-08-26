{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Tests.Utils
  ( within1Second
  , betweenInclusive
  , shouldSatisfyIO
  , checkField
  , checkFailureStatus
  , checkFailureMessage
  , unsafeMkEmailAddress
  , DatabaseName (..)
  , DatabaseConnectionString (..)
  , DatabaseCreationError(..)
  , databaseNameToConnectionString
  , makeDatabase
  , dropTables
  , secondsToMicroseconds
  )
  where

import           Data.ByteString             as BS hiding (putStrLn, reverse,
                                                    unpack)
import qualified Data.ByteString.Char8       as C8 (unpack)

import qualified Data.ByteString.Lazy        as BSL

import           Test.Hspec.Expectations     (Expectation, shouldSatisfy)

import           Servant.Client

import qualified Network.HTTP.Types.Status   as NS

import           Data.Foldable               (forM_)
import           Data.String                 (fromString)
import qualified Data.Text                   as T (unpack)
import           Data.Time.Clock             (UTCTime, diffUTCTime)

import           GHC.Stack                   (HasCallStack)

import           Control.Exception
import           Control.Monad.Except        (ExceptT (..), throwError, unless,
                                              void)
import           Control.Monad.IO.Class

import           Database.Beam.Postgres      (Postgres)
import           Database.Beam.Schema.Tables (Database, DatabaseSettings)
import           Database.PostgreSQL.Simple

import           Mirza.Common.Utils

import           System.Exit
import           System.Process


--------------------------------------------------------------------------------
-- Generic Predicate Utils
--------------------------------------------------------------------------------

-- Checks that the two times are within 1 second of each other.
within1Second :: UTCTime -> UTCTime -> Bool
within1Second expected actual = abs (diffUTCTime expected actual) < 1 -- second


-- | Checks whether a value is between two bounds.
-- | The comparision is done inclusive of the two bounds.
betweenInclusive :: Ord a
                 => a     -- ^ One of the bounds to check
                 -> a     -- ^ The other bound to check.
                 -> a     -- ^ The value to check if it exists between the two bounds.
                 -> Bool  -- ^ Whether the value is between the two bounds inclusive of the bounds.
betweenInclusive bound1 bound2 x = (bound1 `comparator` x) && (x `comparator` bound2) where
  comparator | bound1 <= bound2  = (<=)
             | otherwise         = (>=)


shouldSatisfyIO :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyIO` p = action >>= (`shouldSatisfy` p)


-- Test helper function that enables a predicate to be run on the result of a
-- test call.
checkField :: (a -> b) -> (b -> Bool) -> Either c a -> Bool
checkField accessor predicate = either (const False) (predicate . accessor)


checkFailureStatus :: NS.Status -> Either ServantError a -> Bool
checkFailureStatus = checkFailureField responseStatusCode


checkFailureMessage :: BSL.ByteString -> Either ServantError a -> Bool
checkFailureMessage = checkFailureField responseBody


checkFailureField :: (Eq a) => (Response -> a) -> a -> Either ServantError b -> Bool
checkFailureField accessor x (Left (FailureResponse failure)) = x == (accessor failure)
checkFailureField _        _ _                                = False


--------------------------------------------------------------------------------
-- Database Utils
--------------------------------------------------------------------------------

newtype DatabaseName = DatabaseName
  { getDatabaseName :: ByteString
  }

newtype DatabaseConnectionString = DatabaseConnectionString
  { getDatabaseConnectionString :: ByteString
  }

databaseNameToConnectionString :: DatabaseName -> DatabaseConnectionString
databaseNameToConnectionString dbName = DatabaseConnectionString $ "dbname=" <> (getDatabaseName dbName)


data DatabaseCreationError = DatabaseCreationError deriving (Eq, Show)


-- | Makes sure the database exists (or fails). If the database exists nothing
-- is done, if the database doesn't exist it is created if possible.
makeDatabase :: DatabaseName -> ExceptT DatabaseCreationError IO ()
makeDatabase databaseName = do
  exits <- liftIO $ databaseExists $ databaseNameToConnectionString databaseName
  unless exits $ createDatabase databaseName


databaseExists :: DatabaseConnectionString -> IO Bool
databaseExists databaseConnectionString =
  -- This function is effectively an interface adapter turning the interface
  -- from an open/close -> exception is thrown interface to a bool interface
  -- where the result is True when an exception is not thrown and False when
  -- one is.
  bracket (openConnection databaseConnectionString)
          closeConnection
          (const $ pure True)
    `catch`
      ioExceptionIsFalse
      where
    -- We expect an exception when the database doesn't exist and this is
    -- a normal mode of operation. Unfortunately we can't disambiguate between
    -- when the database exists and we fail for another reason because in all
    -- cases an exception of the same type is thrown and we don't want to make
    -- this code brittle by matching based on the string.
    ioExceptionIsFalse :: IOException -> IO Bool
    ioExceptionIsFalse _ = pure False


openConnection :: DatabaseConnectionString -> IO Connection
openConnection (DatabaseConnectionString databaseConnectionString) = connectPostgreSQL databaseConnectionString


closeConnection :: Connection -> IO ()
closeConnection = close


createDatabase :: DatabaseName -> ExceptT DatabaseCreationError IO ()
createDatabase (DatabaseName databaseName) = do
  processHandle <- liftIO $ spawnProcess "createdb" [C8.unpack databaseName]
  exitCode <- liftIO $ waitForProcess processHandle
  case exitCode of
    ExitSuccess -> pure ()
    _           -> throwError DatabaseCreationError


-- | Drop all tables currently specified in the table definition.
-- Note: This potentially means that legacy tables that no longer exist in the
--       current schema maybe retained accidentally. This function should be
--       updated accordingly once we support proper migrations.
dropTables :: Database Postgres db => DatabaseSettings Postgres db -> Connection -> IO ()
dropTables db conn = do
  -- Reversing the list because tables at the top are dependencies of tables
  -- at the bottom, and they need to be dropped first
  let tables = reverse $ getTableNames db
  -- Note: Its not clear why it is seemingly ok to remove tables in apparently
  --       arbitrary order which might contain primary keys that are referenced
  --       as foreign keys from other tables and that postgres doesn't complain
  --       about this. If this function breaks in the future this could be worth
  --       investigating.
  void $ forM_ tables $ \tableName -> do
    execute_ conn $ fromString $ T.unpack $ "DROP TABLE IF EXISTS " <> tableName <> ";"


-- | Converts from number of seconds to the number of microseconds.
secondsToMicroseconds :: (Num a) => a -> a
secondsToMicroseconds = (* 1000000)
