{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.BeamQueries where

import           Test.Hspec

import           BeamQueries
import           QueryUtils
import           Dummies
import           Database.PostgreSQL.Simple
import           Database.Beam.Backend.Types (Auto (..))
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam
import           Control.Lens

import           Data.UUID (fromString, nil)
import           Data.Maybe (fromJust)
import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           Crypto.Scrypt

import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.GS1.Event (allEventTypes)
import           Data.Either
import           Data.GS1.EPC
import qualified Data.GS1.Event as Ev

import           Utils
import           AppConfig (runAppM, Env, AppM, runDb)
import qualified StorageBeam as SB
import qualified Model as M



-- NOTE in this file, where fromJust is used in the tests, it is because we expect a Just... this is part of the test
-- NOTE tables dropped after every running of test in an "it"

-- for grabbing the encrypted password from user 1
hashIO :: MonadIO m => m ByteString
hashIO = getEncryptedPass <$> (liftIO $ encryptPassIO' (Pass $ encodeUtf8 $ M.password dummyNewUser))

testQueries :: SpecWith (Connection, Env)
testQueries = do
  describe "newUser tests" $ do
    it "newUser test 1" $ \(conn, env) -> do
        hash <- hashIO
        uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
        user <- fromRight' <$> (runAppM env $ selectUser uid)

        --print $ show hash
        --print $ show $ SB.password_hash $ fromJust user

        (fromJust user) `shouldSatisfy`
            (\u ->
              (SB.phone_number u) == (M.phoneNumber dummyNewUser) &&
              (SB.email_address u) == (M.emailAddress dummyNewUser) &&
              (SB.first_name u) == (M.firstName dummyNewUser) &&
              (SB.last_name u) == (M.lastName dummyNewUser) &&
              (SB.user_biz_id u) == (SB.BizId (M.company dummyNewUser)) &&
              -- note database bytestring includes the salt, this checks password
              (verifyPass' (Pass $ encodeUtf8 $ M.password dummyNewUser) (EncryptedPass $ SB.password_hash u)) &&
              (SB.user_id u) == uid)

  describe "authCheck tests" $ do
    it "authCheck test 1" $ \(conn, env) -> do
      --hash <- hashIO
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress dummyNewUser) (encodeUtf8 $ M.password dummyNewUser)) --hash)
      (fromJust user) `shouldSatisfy`
        (\u -> (M.userId u) == uid &&
               (M.userFirstName u) == (M.firstName dummyNewUser) &&
               (M.userLastName u) == (M.lastName dummyNewUser))

  describe "Object Event" $ do
    it "Insert Object Event" $ \(conn, env) -> do
      eventId <- fromRight' <$> (runAppM env $ insertObjectEvent dummyUser dummyObjectEvent)
      insertedEvent <- fromRight' <$> (runAppM env $ findEvent eventId)
      (fromJust insertedEvent) `shouldSatisfy`
        (\ev -> ev == dummyEvent)

    it "List event" $ \(conn, env) -> do
      eventId <- fromRight' <$> (runAppM env $ insertObjectEvent dummyUser dummyObjectEvent)
      insertedEvent <- fromRight' <$> (runAppM env $ findEvent eventId)
      eventList <- fromRight' <$> (runAppM env $ listEvents dummyLabelEpc)
      eventList `shouldBe` [fromJust insertedEvent]

  -- liftIO $ print objEvent
  -- mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (rights allParsedEvents)


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
