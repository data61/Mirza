{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Mirza.SupplyChain.Tests.Service
  ( testServiceQueries
  ) where

import           Mirza.SupplyChain.Tests.Dummies

import           Mirza.SupplyChain.EventUtils
import           Mirza.SupplyChain.Handlers.EventRegistration
import           Mirza.SupplyChain.Types                      as ST

import           Data.Either                                  (isLeft)

import           GHC.Stack                                    (HasCallStack)

import           Test.Hspec

testAppM :: context -> AppM context AppError a -> IO a
testAppM scsContext act = runAppM scsContext act >>= \case
    Left err -> fail (show err)
    Right a -> pure a

testServiceQueries :: HasCallStack => SpecWith SCSContext
testServiceQueries = do

  describe "Object Event" $ do
    it "Insert Object Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $
        insertGS1Event dummyObjEvent
      (eventInfoEvent evInfo) `shouldBe` dummyObjEvent

    it "Should not allow duplicate Object events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyObjEvent
      res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyObjEvent
      res `shouldSatisfy` isLeft

  describe "Aggregation Event" $ do
    it "Insert Aggregation Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ insertGS1Event dummyAggEvent
      (eventInfoEvent evInfo) `shouldBe` dummyAggEvent

    it "Should not allow duplicate Aggregation events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyAggEvent
      res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyAggEvent
      res `shouldSatisfy` isLeft

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ insertGS1Event dummyTransfEvent
      (eventInfoEvent evInfo) `shouldBe` dummyTransfEvent

    it "Should not allow duplicate Transformation events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyTransfEvent
      res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyTransfEvent
      res `shouldSatisfy` isLeft

  describe "Transaction Event" $ do
  -- The otherUserIds are being stubbed out here
    it "Insert Transaction Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ insertGS1Event dummyTransactEvent
      (eventInfoEvent evInfo) `shouldBe` dummyTransactEvent

    it "Should not allow duplicate Transaction events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyTransactEvent
      res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyTransactEvent
      res `shouldSatisfy` isLeft

  describe "Association Event" $ do
 
    it "Insert Association Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ insertGS1Event dummyAssocEvent
      (eventInfoEvent evInfo) `shouldBe` dummyAssocEvent

    it "Should not allow duplicate Transaction events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyAssocEvent
      res <- runAppM @_ @AppError scsContext $ insertGS1Event dummyAssocEvent
      res `shouldSatisfy` isLeft

  describe "DWhere" $
    it "Insert and find DWhere" $ \scsContext -> do
      insertedDWhere <- testAppM scsContext $ do
        (_, eventId) <- insertGS1Event dummyObjEvent
        runDb $ findDWhere eventId
      insertedDWhere `shouldBe` Just dummyDWhere
