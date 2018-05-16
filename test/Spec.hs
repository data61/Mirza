
module Main where

import           Mirza.SupplyChain.Migrate
import           Mirza.SupplyChain.Types    as AC

import           Test.Hspec.Core            (sequential)
import           Test.Tasty                 hiding (withResource)
import           Test.Tasty.Hspec           (around, testSpec)
import           Test.Tasty.Runners         (NumThreads (..))

import           Tests.Client
import           Tests.Service

import           Control.Exception          (bracket)
import           Data.Int
import           Database.Beam.Postgres
import           Database.PostgreSQL.Simple

import           Crypto.Scrypt              (defaultParams)
import           Data.Pool                  (destroyAllResources, withResource)

-- dbFunc = withDatabaseDebug putStrLn

-- INTERESTING NOTE ON MIGRATION
-- receive this error if the tables already exist (not in tests anymore since delete them beforehand)
--  uncaught exception: ErrorCall (Data.Either.Combinators.fromRight: Argument takes form 'Left _'
--  CallStack (from HasCallStack):
--    error, called at src/Data/Either/Combinators.hs:106:24 in either-4.4.1.1-6PiwKYkn4v6B4KO2R2Fu1b:Data.Either.Combinators)

-- drop all tables created by migration. Equivalent to, at the time of writing;
-- execute_ conn "DROP TABLE IF EXISTS users, keys, businesses, contacts, labels, what_labels, items, transformations, locations, events, whats, \"bizTransactions\", whys, wheres, whens, \"labelEvents\", \"userEvents\", hashes, blockchain;"
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

openConnection :: IO SCSContext
openConnection = do
  connpool <- defaultPool
  _ <- withResource connpool dropTables -- drop tables before so if already exist no problems... means tables get overwritten though
  withResource connpool (tryCreateSchema True)
  let envT = AC.mkEnvType True
      env  = AC.SCSContext envT connpool defaultParams
  return env

closeConnection :: SCSContext -> IO ()
closeConnection env =
  destroyAllResources (AC._dbConnPool env)

withDatabaseConnection :: (SCSContext -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

main :: IO ()
main = do
  hspecTests <- testSpec "HSpec" (sequential $ around withDatabaseConnection testQueries)
  clientTests <- testSpec "Client HSpec" clientSpec

  defaultMain $ localOption (NumThreads 1) $ testGroup "tests"
    [ hspecTests
    , clientTests
    ]
