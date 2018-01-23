{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.BeamQueries where

import           Test.Hspec
import           Database.PostgreSQL.Simple

import           BeamQueries
import           Model

import Data.UUID (fromString)
import Data.Maybe (fromJust)
import StorageBeam (PrimaryKeyType)

import Database.Beam.Backend.Types (Auto (..))

testNewUser :: SpecWith Connection
testNewUser = do
  describe "newUser" $ do
    it "newUser1" $ \conn -> do
      uid <- newUser conn (NewUser "000" "fake@gmail.com" "Bob" "Smith" (fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c") "password")
      uid `shouldBe` (Auto $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c")
      --((Auto (fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"))::PrimaryKeyType)

    -- TODO = fix... bug
    -- it "newUser2" $ \conn -> do
    --   uid_blah <- newUser conn (NewUser "000" "fake@gmail.com" "Bob" "Smith" 42 "password")
    --   uid <- newUser conn (NewUser "001" "fake1@gmail.com" "Bob1" "Smith1" 421 "password1")
    --   uid `shouldBe` 1

  -- describe "insertUser" $ do
  --   it "insertUser1" $ \conn -> do
  --     uid <- 