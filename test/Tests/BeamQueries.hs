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

import Control.Monad.IO.Class
import Data.Maybe
import AppConfig (runAppM, Env)
import Data.Either.Combinators

testNewUser :: SpecWith (Connection, Env)
testNewUser = do
  describe "newUser" $ do
    it "newUser1" $ \(conn, env) ->
      let uid_check = (fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c") in do
        uid <- fromRight' <$> (runAppM env $ newUser (NewUser "000" "fake@gmail.com" "Bob" "Smith" "blah Ltd" "password"))
        uid `shouldBe` uid_check