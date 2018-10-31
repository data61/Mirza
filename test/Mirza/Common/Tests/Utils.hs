{-# LANGUAGE FlexibleContexts #-}

-- | General utility functions used throughout the codebase
module Mirza.Common.Tests.Utils
  ( within1Second
  , betweenInclusive
  , shouldSatisfyIO
  , unsafeMkEmailAddress
  , DatabaseName (..)
  , DatabaseConnectionString (..)
  , DatabaseCreationError(..)
  , databaseNameToConnectionString
  , makeDatabase
  , dropTables
  )
  where

import           Data.Maybe              (fromJust)

import           Data.ByteString         as BS hiding (unpack, putStrLn)
import qualified Data.ByteString.Char8   as C8 (unpack)

import           Text.Email.Validate     (EmailAddress, emailAddress)

import           Test.Hspec.Expectations (Expectation, shouldSatisfy)

import           Data.Foldable           (forM_)
import           Data.String             (fromString)
import qualified Data.Text               as T (unpack)
import           Data.Time.Clock         (UTCTime, diffUTCTime)

import           GHC.Stack               (HasCallStack)

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Except    (ExceptT (..), throwError, unless, void)

import           Database.PostgreSQL.Simple
import           Database.Beam.Schema.Tables (Database, DatabaseSettings)
import           Database.Beam.Postgres      (Postgres)

import           Mirza.Common.Utils

import           System.Process
import           System.Exit


--------------------------------------------------------------------------------
-- Generic Predicate Utils
--------------------------------------------------------------------------------

-- Checks that the two times are within 1 second of each other.
within1Second :: UTCTime -> UTCTime -> Bool
within1Second expected actual = abs (diffUTCTime expected actual) < 1 -- second


-- | Checks whether a value is between two bounds.
-- | The comparision is done inclusive of the two bounds.
betweenInclusive :: Ord a =>
                       a     -- ^ One of the bounds to check
                    -> a     -- ^ The other bound to check.
                    -> a     -- ^ The value to check if it exists between the two bounds.
                    -> Bool  -- ^ Whether the value is between the two bounds inclusive of the bounds.
betweenInclusive bound1 bound2 x = (bound1 `comparitor` x) && (x `comparitor` bound2) where
  comparitor | bound1 <= bound2  = (<=)
             | otherwise         = (>=)


--------------------------------------------------------------------------------
-- Email Utils
--------------------------------------------------------------------------------

shouldSatisfyIO :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyIO` p = action >>= (`shouldSatisfy` p)

-- | Only use this with hardcodes email addresses that are guaranteed to return
-- a ``Just``
unsafeMkEmailAddress :: BS.ByteString -> EmailAddress
unsafeMkEmailAddress = fromJust . emailAddress


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
  let tables = getTableNames db
  void $ forM_ tables $ \tableName -> do
    execute_ conn $ fromString $ T.unpack $ "DROP TABLE IF EXISTS " <> tableName <> ";"
