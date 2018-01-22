{-# LANGUAGE DataKinds #-}

module Tests.BeamQueries where

import           Test.Hspec
import           Database.PostgreSQL.Simple

import           BeamQueries

testNewUser :: SpecWith Connection
testNewUser = do
  describe "blah" $ do
    it "blah" $ \c -> do
      
      1 `shouldBe` 1
