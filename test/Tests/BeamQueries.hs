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

import Data.Text.Encoding (encodeUtf8)
import           Crypto.Scrypt
import Data.ByteString

-- NOTE in this file, where fromJust is used in the tests, it is because we expect a Just... tis is part of the test
-- NOTE tables dropped after every running of test in an "it"

user1 = (M.NewUser "000" "fake@gmail.com" "Bob" "Smith" "blah Ltd" "password")

-- for grabbing the encrypted password from user 1
hashIO :: MonadIO m => m ByteString
hashIO = getEncryptedPass <$> (liftIO $ encryptPassIO' (Pass $ encodeUtf8 $ M.password user1))

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

testQueries :: SpecWith (Connection, Env)
testQueries = do
  describe "newUser tests" $ do
    it "newUser test 1" $ \(conn, env) -> do
        hash <- hashIO
        uid <- fromRight' <$> (runAppM env $ newUser user1)
        user <- fromRight' <$> (runAppM env $ selectUser uid)

        --print $ show hash
        --print $ show $ SB.password_hash $ fromJust user

        (fromJust user) `shouldSatisfy` (\u -> (SB.phone_number u) == (M.phoneNumber user1) &&
                                               (SB.email_address u) == (M.emailAddress user1) &&
                                               (SB.first_name u) == (M.firstName user1) &&
                                               (SB.last_name u) == (M.lastName user1) &&
                                               (SB.user_biz_id u) == (SB.BizId (M.company user1)) &&
                                               --(SB.password_hash u) == hash && --(encodeUtf8 $ M.password user1) &&
                                               (SB.user_id u) == uid)

  -- describe "authCheck tests" $ do
  --   it "authCheck test 1" $ \(conn, env) -> do
  --     --hash <- hashIO
  --     uid <- fromRight' <$> (runAppM env $ newUser user1)
  --     user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress user1) (encodeUtf8 $ M.password user1)) --hash)
  --     (fromJust user) `shouldSatisfy` (\u -> (M.userId u) == uid &&
  --                                            (M.userFirstName u) == (M.firstName user1) &&
  --                                            (M.userLastName u) == (M.lastName user1))





  -- describe "addPublicKey tests" $ do
  --   it "addPublicKey test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "getPublicKey tests" $ do
  --   it "getPublicKey test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "getPublicKeyInfo tests" $ do
  --   it "getPublicKeyInfo test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "getUser tests" $ do
  --   it "getUser test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertDWhat tests" $ do
  --   it "insertDWhat test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertDWhen tests" $ do
  --   it "insertDWhen test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertDWhy tests" $ do
  --   it "insertDWhy test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "eventCreateObject tests" $ do
  --   it "eventCreateObject test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1