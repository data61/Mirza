{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.BeamQueries where

import           Test.Hspec

import           BeamQueries
import           Database.PostgreSQL.Simple
import           Database.Beam.Backend.Types (Auto (..))
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.Beam
import           Control.Lens

import           Data.UUID (fromString)
import           Data.Maybe (fromJust)
import           Control.Monad.IO.Class
import           Data.Either.Combinators
import           Crypto.Scrypt

import           Data.ByteString
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import           AppConfig (runAppM, Env, AppM, runDb)
import qualified StorageBeam as SB
import qualified Model as M

-- NOTE in this file, where fromJust is used in the tests, it is because we expect a Just... this is part of the test
-- NOTE tables dropped after every running of test in an "it"

dummyNewUser :: M.NewUser
dummyNewUser = M.NewUser "000" "fake@gmail.com" "Bob" "Smith" "blah Ltd" "password"

sampleObjectFile :: FilePath
sampleObjectFile = "../GS1Combinators/test/test-xml/ObjectEvent.xml"

dummyUser :: M.User
dummyUser = M.User nil "Sajid" "Anower"

runEventCreateObject :: FilePath -> AppM ()
runEventCreateObject xmlFile = do
  doc <- liftIO $ Text.XML.readFile def xmlFile
  let mainCursor = fromDocument doc
  -- scope for optimization: only call parseEventByType on existent EventTypes
      allParsedEvents =
        filter (not . null) $ concat $
        parseEventByType mainCursor <$> allEventTypes
      (Right objEvent) = head allParsedEvents
      newObj = M.mkObjectEvent objEvent
  eventId <- eventCreateObject dummyUser newObj
  liftIO $ print eventId
  -- liftIO $ print objEvent
  -- mapM_ (TL.putStrLn . TLE.decodeUtf8 . encodePretty) (rights allParsedEvents)

-- for grabbing the encrypted password from user 1
hashIO :: MonadIO m => m ByteString
hashIO = getEncryptedPass <$> (liftIO $ encryptPassIO' (Pass $ encodeUtf8 $ M.password dummyNewUser))

selectUser :: M.UserID -> AppM (Maybe SB.User)
selectUser uid = do
  r <- runDb $
          runSelectReturningList $ select $ do
          user <- all_ (SB._users SB.supplyChainDb)
          guard_ (SB.user_id user ==. val_ uid)
          pure user
  case r of
    Right [user] -> return $ Just user
    _            -> return Nothing


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

  -- describe "Object Event" $ do
  --   it "eventCreateObject" $ \(conn, env) -> do
  --     runEventCreateObject sampleObjectFile
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