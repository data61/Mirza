{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.Common.Tests.InitClient where

import           Mirza.SupplyChain.Main            (ServerOptions (..),
                                                    initApplication,
                                                    initSCSContext)

import           Control.Concurrent                (ThreadId)

import           Servant.Client                    (BaseUrl)

import           Data.Either                       (isRight)

import           Test.Tasty.Hspec

import           Katip                             (Severity (DebugS))

import           Mirza.SupplyChain.Types           as ST

import           Mirza.Common.Tests.ServantUtils
import           Mirza.SupplyChain.Database.Schema as Schema

import           Database.Beam.Query               (delete, runDelete, val_)

import           Data.ByteString.Char8             (ByteString)

testDbConnStrSCS :: ByteString
testDbConnStrSCS = "dbname=testsupplychainserver"

soSCS :: ServerOptions
soSCS = ServerOptions Dev False testDbConnStrSCS "127.0.0.1" 8000 14 8 1 DebugS

runSCSApp :: IO (ThreadId, BaseUrl)
runSCSApp = do
  ctx <- initSCSContext soSCS
  let SupplyChainDb
        usersTable
        businessesTable
        contactsTable
        labelsTable
        whatLabelsTable
        itemsTable
        transformationsTable
        locationsTable
        eventsTable
        whatsTable
        bizTransactionsTable
        whysTable
        wheresTable
        whensTable
        labelEventsTable
        userEventsTable
        signaturesTable
        hashesTable
        blockchainTable
          = supplyChainDb
  flushDbResult <- runAppM @_ @ServiceError ctx $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable $ usersTable
      deleteTable $ businessesTable
      deleteTable $ contactsTable
      deleteTable $ labelsTable
      deleteTable $ whatLabelsTable
      deleteTable $ itemsTable
      deleteTable $ transformationsTable
      deleteTable $ locationsTable
      deleteTable $ eventsTable
      deleteTable $ whatsTable
      deleteTable $ bizTransactionsTable
      deleteTable $ whysTable
      deleteTable $ wheresTable
      deleteTable $ whensTable
      deleteTable $ labelEventsTable
      deleteTable $ userEventsTable
      deleteTable $ signaturesTable
      deleteTable $ hashesTable
      deleteTable $ blockchainTable
  flushDbResult `shouldSatisfy` isRight
  startWaiApp =<< initApplication soSCS ctx

