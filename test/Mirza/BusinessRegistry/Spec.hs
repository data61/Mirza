{-# LANGUAGE TypeApplications #-}


module Main where

import           Mirza.Common.Tests.InitClient           (testDbConnStrBR)

import           Mirza.BusinessRegistry.Database.Migrate
import           Mirza.BusinessRegistry.Main             hiding (main)
import           Mirza.BusinessRegistry.Types            as BRT

import           Test.Hspec.Core.Spec                    (sequential)
import           Test.Tasty                              hiding (withResource)
import           Test.Tasty.Hspec                        (around, testSpec)
import           Test.Tasty.Runners                      (NumThreads (..))

import           Mirza.BusinessRegistry.Tests.Business   (testBizQueries)
import           Mirza.BusinessRegistry.Tests.Client
import           Mirza.BusinessRegistry.Tests.Keys       (testKeyQueries)

import           Control.Exception                       (bracket)
import           Data.Int
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple

import           Data.Pool                               (withResource)
import qualified Data.Pool                               as Pool

import           Katip                                   (Severity (DebugS))
import           System.IO.Temp                          (emptySystemTempFile)

-- dbFunc = withDatabaseDebug putStrLn

-- INTERESTING NOTE ON MIGRATION
-- receive this error if the tables already exist (not in tests anymore since delete them beforehand)
--  uncaught exception: ErrorCall (Data.Either.Combinators.fromRight: Argument takes form 'Left _'
--  CallStack (from HasCallStack):
--    error, called at src/Data/Either/Combinators.hs:106:24 in either-4.4.1.1-6PiwKYkn4v6B4KO2R2Fu1b:Data.Either.Combinators)

-- drop all tables created by migration
dropTables :: Connection -> IO Int64
dropTables conn =
  --https://stackoverflow.com/questions/3327312/drop-all-tables-in-postgresql
  execute_ conn "DO $$ DECLARE                                                                              \
               \     r RECORD;                                                                              \
               \ BEGIN                                                                                      \
               \     FOR r IN (SELECT tablename FROM pg_tables WHERE schemaname = current_schema()) LOOP    \
               \         EXECUTE 'DROP TABLE IF EXISTS ' || quote_ident(r.tablename) || ' CASCADE';         \
               \     END LOOP;                                                                              \
               \ END $$;                                                                                    "


defaultPool :: IO (Pool.Pool Connection)
defaultPool = Pool.createPool (connectPostgreSQL testDbConnStrBR) close
                1 -- Number of "sub-pools",
                60 -- How long in seconds to keep a connection open for reuse
                10 -- Max number of connections to have open at any one time



openConnection :: IO BRContext
openConnection = do
  connpool <- defaultPool
  _ <- withResource connpool dropTables -- drop tables before so if already exist no problems... means tables get overwritten though
  tempFile <- emptySystemTempFile "businessRegistryTests.log"
  ctx <- initBRContext (ServerOptionsBR testDbConnStrBR 16 10 4 DebugS (Just tempFile) Dev)
  initRes <- runMigrationWithConfirmation ctx (const (pure Execute))
  case initRes of
    Left err -> print @SqlError err >> error "Database initialisation failed"
    Right () -> pure ctx


closeConnection :: BRContext -> IO ()
closeConnection = Pool.destroyAllResources . BRT._brDbConnPool

withDatabaseConnection :: (BRContext -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = do
  keyTests <- testSpec "HSpec" (sequential $ around withDatabaseConnection testKeyQueries)
  bizTests <- testSpec "HSpec" (sequential $ around withDatabaseConnection testBizQueries)
  clientTests <- clientSpec

  defaultMain $ localOption (NumThreads 1) $ testGroup "tests"
    [ keyTests
    , bizTests
    , clientTests
    ]
