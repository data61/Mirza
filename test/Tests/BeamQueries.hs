{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.BeamQueries where

import           Test.Hspec
import           Database.PostgreSQL.Simple

import           BeamQueries
import qualified Model as M

import Data.UUID (fromString)
import Data.Maybe (fromJust)

import Database.Beam.Backend.Types (Auto (..))

import Control.Monad.IO.Class
import Data.Maybe
import AppConfig (runAppM, Env, AppM, runDb)
import Data.Either.Combinators

import qualified StorageBeam as SB

import           Database.Beam.Backend.SQL.BeamExtensions
import Database.Beam
import Control.Lens

import qualified Data.Text as T

selectUser :: M.UserID -> AppM (Maybe SB.User)
selectUser uid = do
  r <- runDb $
          runSelectReturningList $ select $ do
          user <- all_ (SB._users SB.supplyChainDb)
          guard_ (SB.user_id user ==. val_ uid)
          pure user
  case r of
    Right [user] -> return $ Just user
    _ -> return Nothing

testNewUser :: SpecWith (Connection, Env)
testNewUser = do
  describe "newUser" $ do
    it "newUser1" $ \(conn, env) ->
      let uid_check = (fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c")
          user1 = (M.NewUser "000" "fake@gmail.com" "Bob" "Smith" "blah Ltd" "password") in do
        
        uid <- fromRight' <$> (runAppM env $ newUser user1)
        user <- fromRight' <$> (runAppM env $ selectUser uid)

        print $ show user

        (fromJust user) `shouldSatisfy` (\u -> (SB.phone_number u) == "000" &&
                                               (SB.email_address u) == "fake@gmail.com" &&
                                               (SB.first_name u) == "Bob" &&
                                               (SB.last_name u) == "Smith" &&
                                               (SB.user_biz_id u) == (SB.BizId "blah Ltd") &&
                                               (SB.password_hash u) == "password")
