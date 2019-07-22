{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.Common.Database ( Migration
                             , runMigrationSimple
                             , dropTablesSimple
                             
                             , createTrigger
                             ) where

import Mirza.Common.Types

import Control.Lens (view, _1)
import Control.Monad ( void, forM_ )
import Data.List ( drop, zip )
import Data.String (fromString)
import Data.Text ( unpack )
import Database.PostgreSQL.Simple

type Migration = Connection -> IO ()

runMigrationSimple :: ( Member c '[HasLogging, HasDB], Member err '[AsSqlError]) => c -> [Migration] -> IO (Either err ())
runMigrationSimple c migrations = runAppM c $ runDb $ do
  conn <- view _1
  liftIO $ do
    _ <- execute_ conn "SET client_min_messages = WARNING;"
    n <- getVersion conn
    forM_ (drop n (zip @Int [1..] migrations)) $ \(i, m) -> do
      m conn
      execute conn "INSERT INTO version (number, executed) VALUES (?, now());" (Only i)

dropTablesSimple :: ( Member c '[HasLogging, HasDB], Member err '[AsSqlError]) => c -> IO (Either err ())
dropTablesSimple c = runAppM c $ runDb $ do
  conn <- view _1
  liftIO $ do
    _ <- execute_ conn "SET client_min_messages = WARNING;"
    tables <- query_ conn "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public';"
    forM_ tables $ \t -> execute_ conn $ "DROP TABLE IF EXISTS " <> (fromString (unpack (fromOnly t))) <> " CASCADE;"

getVersion :: Connection -> IO Int
getVersion conn = do
  x <- query_ conn "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_schema = 'public' AND table_name = 'version');"
  case x of
    [ Only True ] -> do
      result <- query_ conn "SELECT number FROM version ORDER BY number DESC LIMIT 1;"
      case result of
        [ Only n ] -> pure n
        _ -> pure 0
    _ -> 0 <$ execute_ conn "CREATE TABLE version (number INTEGER NOT NULL PRIMARY KEY, executed timestamptz NOT NULL);"

createTrigger :: Connection -> Query -> IO ()
createTrigger conn tName = void $ execute_ conn $
      "CREATE OR REPLACE FUNCTION sync_lastmod() RETURNS trigger AS $$ \
      \BEGIN \
        \NEW.last_update := NOW() AT TIME ZONE 'UTC'; \
        \RETURN NEW; \
      \END; \
      \$$ LANGUAGE plpgsql; \
      \DROP TRIGGER IF EXISTS sync_lastmod ON \"" <> tName <> "\";" <>
      "CREATE TRIGGER sync_lastmod \
      \BEFORE UPDATE OR INSERT ON \"" <> tName <>
        "\" FOR EACH ROW EXECUTE PROCEDURE sync_lastmod();"
