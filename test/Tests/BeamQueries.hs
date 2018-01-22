{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.BeamQueries where

import           Test.Hspec
import           Database.PostgreSQL.Simple

import           BeamQueries
import           Model

testNewUser :: SpecWith Connection
testNewUser = do
  describe "newUser" $ do
    it "newUser1" $ \conn -> do
      uid <- newUser conn (NewUser "000" "fake@gmail.com" "Bob" "Smith" 42 "password")
      uid `shouldBe` 0
    it "newUser2" $ \conn -> do
      uid_blah <- newUser conn (NewUser "000" "fake@gmail.com" "Bob" "Smith" 42 "password")
      uid <- newUser conn (NewUser "001" "fake1@gmail.com" "Bob1" "Smith1" 421 "password1")
      uid `shouldBe` 1
