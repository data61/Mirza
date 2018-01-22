{-# LANGUAGE DataKinds #-}

module Tests.BeamQueries where

import           Test.Hspec
import           Database.PostgreSQL.Simple

import           BeamQueries
import           Model

testNewUser :: SpecWith Connection
testNewUser = do
  describe "newUser" $ do
    it "newUser" $ \c -> do
      
      1 `shouldBe` 1
