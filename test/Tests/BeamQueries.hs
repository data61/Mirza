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
import Data.Time.Clock (getCurrentTime, UTCTime (..), secondsToDiffTime)
import Data.Time.LocalTime (utc, utcToLocalTime, LocalTime, localTimeToUTC)
import CryptHash (getCryptoPublicKey)
import Data.Binary
import           Data.ByteString.Lazy (toStrict)

import Data.GS1.EPC
import Data.GS1.DWhat
import Data.Time.Calendar (Day (..))

-- NOTE in this file, where fromJust is used in the tests, it is because we expect a Just... tis is part of the test
-- NOTE tables dropped after every running of test in an "it"

user1 :: M.NewUser
user1 = (M.NewUser "000" "fake@gmail.com" "Bob" "Smith" "blah Ltd" "password")
rsaKey1 :: M.RSAPublicKey
rsaKey1 = (M.RSAPublicKey 3 5)
location1 :: LocationEPC
location1 = SGLN "blah Ltd" (LocationReference "11111") Nothing
-- time in here is a UTCTime, i.e. EPCISTime
object1 :: M.NewObject
object1 = (M.NewObject (IL (GIAI "blah Ltd" "123"))
                       (UTCTime (ModifiedJulianDay 1000) (secondsToDiffTime 10000))
                       utc
                       (M.EventLocation location1
                                        location1
                                        (SDLocation, location1)
                                        (SDOwningParty, location1))
                       Nothing)

-- for grabbing the encrypted password from user 1
hashIO :: MonadIO m => m ByteString
hashIO = getEncryptedPass <$> (liftIO $ encryptPassIO' (Pass $ encodeUtf8 $ M.password user1))

timeStampIO :: MonadIO m => m LocalTime
timeStampIO = liftIO $ (utcToLocalTime utc) <$> getCurrentTime

timeStampIOEPCIS :: MonadIO m => m EPCISTime
timeStampIOEPCIS = liftIO $ getCurrentTime

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

selectKey :: M.KeyID -> AppM (Maybe SB.Key)
selectKey keyId = do
  r <- runDb $
          runSelectReturningList $ select $ do
          key <- all_ (SB._keys SB.supplyChainDb)
          guard_ (SB.key_id key ==. val_ keyId)
          pure key
  case r of
    Right [key] -> return $ Just key
    _ -> return Nothing

testQueries :: SpecWith (Connection, Env)
testQueries = do
  describe "newUser tests" $ do
    it "newUser test 1" $ \(conn, env) -> do
        uid <- fromRight' <$> (runAppM env $ newUser user1)
        user <- fromRight' <$> (runAppM env $ selectUser uid)

        (fromJust user) `shouldSatisfy` (\u -> (SB.phone_number u) == (M.phoneNumber user1) &&
                                               (SB.email_address u) == (M.emailAddress user1) &&
                                               (SB.first_name u) == (M.firstName user1) &&
                                               (SB.last_name u) == (M.lastName user1) &&
                                               (SB.user_biz_id u) == (SB.BizId (M.company user1)) &&
                                               -- note database bytestring includes the salt, this checks password
                                               (verifyPass' (Pass $ encodeUtf8 $ M.password user1) (EncryptedPass $ SB.password_hash u)) &&
                                               (SB.user_id u) == uid)

  describe "authCheck tests" $ do
    it "authCheck test 1" $ \(conn, env) -> do
      uid <- fromRight' <$> (runAppM env $ newUser user1)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress user1) (encodeUtf8 $ M.password user1))
      (fromJust user) `shouldSatisfy` (\u -> (M.userId u) == uid &&
                                             (M.userFirstName u) == (M.firstName user1) &&
                                             (M.userLastName u) == (M.lastName user1))

  -- this test seems not to work because of inability by beam to parse time
  describe "addPublicKey tests" $ do
    it "addPublicKey test 1" $ \(conn, env) -> do
      tStart <- timeStampIO
      uid <- fromRight' <$> (runAppM env $ newUser user1)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress user1) (encodeUtf8 $ M.password user1))
      keyId <- fromRight' <$> (runAppM env $ addPublicKey (fromJust user) rsaKey1) -- this is broken

      key <- fromRight' <$> (runAppM env $ selectKey keyId)
      tEnd <- timeStampIO

      (fromJust key) `shouldSatisfy` (\k -> (SB.rsa_public_pkcs8 k) == (toStrict $ encode $ getCryptoPublicKey rsaKey1) &&
                                            (SB.key_id k) == keyId &&
                                            (SB.key_user_id k) == (SB.UserId uid) &&
                                            (SB.creation_time k) > tStart && (SB.creation_time k) < tEnd &&
                                            isNothing (SB.revocation_time k))

  -- this test seems not to work because of inability by beam to parse time
  describe "getPublicKey tests" $ do
    it "getPublicKey test 1" $ \(conn, env) -> do
      fromRight' <$> (runAppM env $ newUser user1)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress user1) (encodeUtf8 $ M.password user1))
      keyId <- fromRight' <$> (runAppM env $ addPublicKey (fromJust user) rsaKey1) -- this is broken
      key <- fromRight' <$> (runAppM env $ getPublicKey keyId)
      key `shouldBe` rsaKey1

  -- this test seems not to work because of inability by beam to parse time
  describe "getPublicKeyInfo tests" $ do
    it "getPublicKeyInfo test 1" $ \(conn, env) -> do
      tStart <- timeStampIOEPCIS
      uid <- fromRight' <$> (runAppM env $ newUser user1)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress user1) (encodeUtf8 $ M.password user1))
      keyId <- fromRight' <$> (runAppM env $ addPublicKey (fromJust user) rsaKey1) -- this is broken
      keyInfo <- fromRight' <$> (runAppM env $ getPublicKeyInfo keyId)
      tEnd <- timeStampIOEPCIS

      keyInfo `shouldSatisfy` (\ki -> (M.userID ki == uid) &&
                                      (M.creationTime ki > tStart && M.creationTime ki < tEnd) &&
                                      isNothing (M.revocationTime ki))

  describe "getUser tests" $ do
    it "getUser test 1" $ \(conn, env) -> do
      uid <- fromRight' <$> (runAppM env $ newUser user1)
      user <- fromRight' <$> (runAppM env $ getUser $ M.emailAddress user1)
      (fromJust user) `shouldSatisfy` (\u -> (M.userId u == uid) &&
                                             (M.userFirstName u == M.firstName user1) &&
                                             (M.userLastName u == M.lastName user1))

  -- describe "insertDWhat tests" $ do
  --   it "insertDWhat test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertDWhen tests" $ do
  --   it "insertDWhen test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertDWhy tests" $ do
  --   it "insertDWhy test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertSrcDestType tests" $ do
  --   it "insertSrcDestType test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertLocationEPC tests" $ do
  --   it "insertLocationEPC test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertDWhere tests" $ do
  --   it "insertDWhere test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertEvent tests" $ do
  --   it "insertEvent test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertUserEvent tests" $ do
  --   it "insertUserEvent test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  describe "eventCreateObject tests" $ do
    it "eventCreateObject test 1" $ \(conn, env) -> do
      fromRight' <$> (runAppM env $ newUser user1)
      user <- fromRight' <$> (runAppM env $ getUser $ M.emailAddress user1)
      eventId <- fromRight' <$> (runAppM env $ eventCreateObject (fromJust user) object1)
      eventId `shouldSatisfy` (\eid -> (True))

  -- describe "toEPCISTime tests" $ do
  --   it "toEPCISTime test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1