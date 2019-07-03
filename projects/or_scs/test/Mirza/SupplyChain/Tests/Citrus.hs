{-# LANGUAGE FlexibleContexts #-}

module Mirza.SupplyChain.Tests.Citrus (
    citrusSpec
  ) where

-- import           Test.Hspec.Expectations
import           Test.Tasty
-- import           Test.Tasty.HUnit

-- import           Mirza.Common.GS1BeamOrphans      (LabelEPCUrn (..))

-- import           Control.Exception               (bracket)
-- import           Control.Monad.Except

-- import           Mirza.SupplyChain.PopulateUtils

-- import           Mirza.Common.Tests.InitClient
-- import           Mirza.Common.Tests.ServantUtils (runClient)


-- import           Data.GS1.EPC

-- import           Mirza.SupplyChain.Client.Servant as SCSClient

-- import           Data.Time                       (getCurrentTime)
-- import           Data.Time.LocalTime             (utc)

-- import           Data.Either                     (isRight)

-- import           Data.HashMap.Lazy               as H

-- =============================================================================
-- Citrus Provenance test
-- =============================================================================

citrusSpec :: IO TestTree
citrusSpec = do
  -- let citrusSupplyChainTests = testCaseSteps "Creating food provenance trail" $ \step ->
  --       bracket runApps endApps $ \testData -> do
  --         let scsUrl = scsBaseUrl testData
  --             httpSCS = runClient scsUrl
  --             orUrl = orBaseUrl testData
  --             orAuthUser = orAuthData testData
  --             initAuthHt = H.empty

  --         step "Initialising the data"
  --         _authHt <- insertAndAuth scsUrl orUrl orAuthUser locationMap initAuthHt allEntities
  --         currTime <- getCurrentTime
  --         let citrusEachEvents = makeCitrusEvents (EPCISTime currTime) utc
  --             _citrusEvents = eachEventEvent <$> citrusEachEvents
  --         insertEachEventResult <- traverse  (httpSCS . insertEachEvent) citrusEachEvents
  --         void $ pure $ (flip shouldSatisfy) isRight <$> insertEachEventResult

          -- step "Listing events with each label"
          -- let (Just (_, farmerAuth, _)) = H.lookup farmerE authHt
          -- print $ renderURL boxLabel
          -- resBox <- httpSCS $ SCSClient.listEvents farmerAuth (LabelEPCUrn . renderURL $ boxLabel)
          -- resBox `shouldSatisfy` isRight
          -- let Right boxEvents = resBox
          -- print $ length boxEvents
          -- let [evBox] = boxEvents
          -- evBox `shouldBe` citrusEvents !! 7

          -- let Just boxParent = getParent . _what $ evBox
          -- boxParent `shouldBe` ParentLabel palletLabel
          -- Right resPallet <- httpSCS $ SCSClient.listEvents farmerAuth (LabelEPCUrn . renderURL $ boxParent)
          -- let [pallet1, pallet2, pallet3] = resPallet
          -- for_ resPallet (\i -> putStrLn $ "\n========\n" <> show i <> "\n========\n")
          -- length (nub resPallet) `shouldBe` 3

          -- Right resLand <- httpSCS $ SCSClient.listEvents farmerAuth (ST.LabelEPCUrn . renderURL $ landLabel)
          -- length resBox `shouldBe` 1

          -- step "check eventInfo for each event"

  pure $ testGroup "Citrus Client tests"
        -- TODO: Reinclude the following test case which fails because we have not sorted out auth for test cases yet.
        [ -- citrusSupplyChainTests
        ]
