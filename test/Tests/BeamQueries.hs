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
import AppConfig (runAppM, Env, AppM, runDb)
import Data.Either.Combinators

import qualified StorageBeam as SB

import           Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam
import Control.Lens

selectUser :: UserID -> AppM (Maybe User)
selectUser uid = do
  r <- runDb $
          runSelectReturningList $ select $ do
          user <- all_ (SB._users SB.supplyChainDb)
          guard_ (SB.user_id user ==. val_ uid)
          pure user
  case r of
    Right [user] -> return $ Just $ userTableToModel user
    _ -> return Nothing

testNewUser :: SpecWith (Connection, Env)
testNewUser = do
  describe "newUser" $ do
    it "newUser1" $ \(conn, env) ->
      let uid_check = (fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c")
          user1 = (NewUser "000" "fake@gmail.com" "Bob" "Smith" "blah Ltd" "password") in do
        uid <- fromRight' <$> (runAppM env $ newUser user1)
        user <- fromRight' <$> (runAppM env $ selectUser uid)
        --user `shouldBe` user1
        --user1 `shouldBe` (fromJust user)
        print "blasddd"
        print $ show user
        --user `shouldBe` user
        (fromJust user) `shouldSatisfy`(\u -> (userFirstName u) == "Bob")