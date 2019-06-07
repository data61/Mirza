{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.OrgRegistry.Tests.Org
  ( testOrgQueries
  ) where


import           Mirza.OrgRegistry.Handlers.Org
import           Mirza.OrgRegistry.Types             as ORT

import           GHC.Stack                                (HasCallStack)

import           Test.Hspec

testAppM :: context
         -> AppM context ORError a
         -> IO a
testAppM orContext act = runAppM orContext act >>= \case
    Left err -> fail (show err)
    Right a -> pure a

testOrgQueries :: (HasCallStack) => SpecWith ORT.ORContextMinimal
testOrgQueries = do

  describe "Org" $ do
    it "List Org empty" $ \orContext -> do
      orgList <- testAppM orContext (searchOrgs Nothing Nothing Nothing)
      orgList `shouldBe` []
