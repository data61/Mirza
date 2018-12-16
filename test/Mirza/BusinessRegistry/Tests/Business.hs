{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.BusinessRegistry.Tests.Business
  ( testBizQueries
  ) where


import           Mirza.BusinessRegistry.Handlers.Business
import           Mirza.BusinessRegistry.Types             as BT

import           GHC.Stack                                (HasCallStack)

import           Test.Hspec

testAppM :: context
         -> AppM context BRError a
         -> IO a
testAppM brContext act = runAppM brContext act >>= \case
    Left err -> fail (show err)
    Right a -> pure a

testBizQueries :: (HasCallStack) => SpecWith BT.BRContext
testBizQueries = do

  describe "Business" $ do
    it "List Business empty" $ \brContext -> do
      bizList <- testAppM brContext (searchBusinesses Nothing Nothing Nothing)
      bizList `shouldBe` []
