{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.BeamQueries where

import           Test.Hspec

import           BeamQueries
import           QueryUtils
import           Dummies
import           Database.PostgreSQL.Simple
import           Database.Beam

import           Data.Maybe (fromJust, isNothing)
import           Data.Either.Combinators
import           Crypto.Scrypt

-- import           Data.Text.Encoding (encodeUtf8)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text.Encoding (encodeUtf8)
import           Data.GS1.Event (allEventTypes)
import           Data.GS1.EPC
import           Data.UUID (nil)
-- import qualified Data.GS1.Event as Ev

import           Utils
import           AppConfig (runAppM, Env(..), AppM, runDb, EnvType(..))
import qualified StorageBeam as SB
import qualified Model as M
import           Migrate (testDbConnStr)

-- import           Crypto.Scrypt
import           Data.Time.Clock (getCurrentTime, UTCTime(..))
import           Data.Time (ZonedTime(..), utcToZonedTime
                           , zonedTimeToUTC)
import           Data.Time.LocalTime (utc, utcToLocalTime, LocalTime)
import           CryptHash (getCryptoPublicKey)
import           Data.Binary
import           Database.PostgreSQL.Simple (execute_)

-- NOTE in this file, where fromJust is used in the tests, it is because we expect a Just... this is part of the test
-- NOTE tables dropped after every running of test in an "it"

-- for grabbing the encrypted password from user 1
hashIO :: MonadIO m => m ByteString
hashIO = getEncryptedPass <$> (liftIO $ encryptPassIO' (Pass $ encodeUtf8 $ M.password dummyNewUser))

timeStampIO :: MonadIO m => m ZonedTime
timeStampIO = liftIO $ (utcToZonedTime utc) <$> getCurrentTime

timeStampIOEPCIS :: MonadIO m => m EPCISTime
timeStampIOEPCIS = liftIO $ getCurrentTime


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
      hash <- hashIO
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      user <- fromRight' <$> (runAppM env $ selectUser uid)

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
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress dummyNewUser) (encodeUtf8 $ M.password dummyNewUser)) --hash)
      (fromJust user) `shouldSatisfy`
        (\u -> (M.userId u) == uid &&
               (M.userFirstName u) == (M.firstName dummyNewUser) &&
               (M.userLastName u) == (M.lastName dummyNewUser))

  describe "addPublicKey tests" $ do
    it "addPublicKey test 1" $ \(conn, env) -> do
      tStart <- timeStampIO
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      storageUser <- fromRight' <$> (runAppM env $ selectUser uid)
      let user = userTableToModel . fromJust $ storageUser
      keyId <- fromRight' <$> (runAppM env $ addPublicKey user dummyRsaPubKey) -- this is broken

      key <- fromRight' <$> (runAppM env $ selectKey keyId)
      tEnd <- timeStampIO

      (fromJust key) `shouldSatisfy` (\k -> (SB.rsa_public_pkcs8 k) == (toStrict $ encode $ getCryptoPublicKey dummyRsaPubKey) &&
                                            (SB.key_id k) == keyId &&
                                            (SB.key_user_id k) == (SB.UserId uid) &&
                                            -- (SB.creation_time k) > tStart && (SB.creation_time k) < tEnd &&
                                            isNothing (SB.revocation_time k))

  describe "getPublicKey tests" $ do
    it "getPublicKey test 1" $ \(conn, env) -> do
      fromRight' <$> (runAppM env $ newUser dummyNewUser)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress dummyNewUser) (encodeUtf8 $ M.password dummyNewUser))
      keyId <- fromRight' <$> (runAppM env $ addPublicKey (fromJust user) dummyRsaPubKey) -- this is broken
      key <- fromRight' <$> (runAppM env $ getPublicKey keyId)
      key `shouldBe` dummyRsaPubKey

  describe "getPublicKeyInfo tests" $ do
    it "getPublicKeyInfo test 1" $ \(conn, env) -> do
      tStart <- timeStampIOEPCIS
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress dummyNewUser) (encodeUtf8 $ M.password dummyNewUser))
      keyId <- fromRight' <$> (runAppM env $ addPublicKey (fromJust user) dummyRsaPubKey) -- this is broken
      keyInfo <- fromRight' <$> (runAppM env $ getPublicKeyInfo keyId)
      tEnd <- timeStampIOEPCIS

      -- keyInfo `shouldSatisfy` (\k -> (SB.rsa_public_pkcs8 k) == (toStrict $ encode $ getCryptoPublicKey dummyRsaPubKey) &&
      --                                       (SB.key_id k) == keyId &&
      --                                       (SB.key_user_id k) == (SB.UserId uid) &&
      --                                       (SB.creation_time k) > tStart && (SB.creation_time k) < tEnd &&
      --                                       isNothing (SB.revocation_time k))
      keyInfo `shouldSatisfy` (\ki -> (M.userID ki == uid) &&
                                      (M.creationTime ki > tStart && M.creationTime ki < tEnd) &&
                                      isNothing (M.revocationTime ki))

  describe "Object Event" $ do
    it "Insert Object Event" $ \(conn, env) -> do
      eventId <- fromRight' <$> (runAppM env $ insertObjectEvent dummyUser dummyObject)
      insertedEvent <- fromRight' <$> (runAppM env $ findEvent eventId)
      (fromJust insertedEvent) `shouldSatisfy`
        (\ev -> ev == dummyObjEvent)

    it "List event" $ \(conn, env) -> do
      eventId <- fromRight' <$> (runAppM env $ insertObjectEvent dummyUser dummyObject)
      insertedEvent <- fromRight' <$> (runAppM env $ findEvent eventId)
      eventList <- fromRight' <$> (runAppM env $ listEvents dummyLabelEpc)
      (fromJust insertedEvent) `shouldSatisfy`
        (\ev -> ev == dummyObjEvent)
      eventList `shouldBe` [fromJust insertedEvent]

  describe "Dis/Aggregation Event" $ do
    it "Insert Aggregation Event" $ \(conn, env) -> do
      eventId <- fromRight' <$> (runAppM env $ insertAggEvent dummyUser dummyAggregation)
      insertedEvent <- fromRight' <$> (runAppM env $ findEvent eventId)
      (fromJust insertedEvent) `shouldSatisfy`
        (\ev -> ev == dummyAggEvent)

    it "List event" $ \(conn, env) -> do
      eventId <- fromRight' <$> (runAppM env $ insertAggEvent dummyUser dummyAggregation)
      insertedEvent <- fromRight' <$> (runAppM env $ findEvent eventId)
      (fromJust insertedEvent) `shouldSatisfy`
        (\ev -> ev == dummyAggEvent)
      eventList <- fromRight' <$> (runAppM env $ listEvents dummyLabelEpc)
      eventList `shouldBe` [fromJust insertedEvent]

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \(conn, env) -> do
      eventId <- fromRight' <$> (runAppM env $ insertTransfEvent dummyUser dummyTransformation)
      
      insertedEvent <- fromRight' <$> (runAppM env $ findEvent eventId)
      (fromJust insertedEvent) `shouldSatisfy`
        (\ev -> ev == dummyTransfEvent)

    it "List event" $ \(conn, env) -> do
      eventId <- fromRight' <$> (runAppM env $ insertTransfEvent dummyUser dummyTransformation)
      insertedEvent <- fromRight' <$> (runAppM env $ findEvent eventId)
      (fromJust insertedEvent) `shouldSatisfy`
        (\ev -> ev == dummyTransfEvent)
      eventList <- fromRight' <$> (runAppM env $ listEvents dummyLabelEpc)
      eventList `shouldBe` [fromJust insertedEvent]

  describe "getUser tests" $ do
    it "getUser test 1" $ \(conn, env) -> do
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      user <- fromRight' <$> (runAppM env $ getUser $ M.emailAddress dummyNewUser)
      (fromJust user)
        `shouldSatisfy`
          (\u ->
            (M.userId u == uid) &&
            (M.userFirstName u == M.firstName dummyNewUser) &&
            (M.userLastName u == M.lastName dummyNewUser)
          )

  (after_ clearContact) . describe "Contacts" $ do
    describe "Add contact" $ do
      it "addContact simple" $ \(conn, env) -> do
        uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
        user <- fromRight' <$> (runAppM env $ getUser $ M.emailAddress dummyNewUser)
        let myContact = makeDummyNewUser "first@gmail.com"
        myContactUid <- fromRight' <$> (runAppM env $ newUser myContact)
        hasBeenAdded <- fromRight' <$> (runAppM env $ addContact (fromJust user) myContactUid)
        hasBeenAdded `shouldBe` True
        isContact <- fromRight' <$> (runAppM env $ isExistingContact uid myContactUid)
        isContact `shouldBe` True

    describe "Remove contact" $ do
      it "Simple remove one" $ \(conn, env) -> do

        -- Adding the contact first
        uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
        mUser <- fromRight' <$> (runAppM env $ getUser $ M.emailAddress dummyNewUser)
        let myContact = makeDummyNewUser "first@gmail.com"
            user = fromJust mUser
        myContactUid <- fromRight' <$> (runAppM env $ newUser myContact)
        hasBeenAdded <- fromRight' <$> (runAppM env $ addContact user myContactUid)
        hasBeenAdded `shouldBe` True

        -- removing the contact now
        hasBeenRemoved <- fromRight' <$> (runAppM env $ removeContact user myContactUid)
        hasBeenRemoved `shouldBe` True
      it "Remove wrong contact" $ \(conn, env) -> do

        -- Adding the user first
        uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
        -- retrieving the user
        mUser <- fromRight' <$> (runAppM env $ getUser $ M.emailAddress dummyNewUser)

        -- Add a new user who is NOT a contact
        otherUserId <- fromRight' <$> (runAppM env $ newUser $ makeDummyNewUser "other@gmail.com")
        let myContactUser = makeDummyNewUser "first@gmail.com"
            user = fromJust mUser
        myContactUid <- fromRight' <$> (runAppM env $ newUser myContactUser)
        hasBeenAdded <- fromRight' <$> (runAppM env $ addContact user myContactUid)
        hasBeenAdded `shouldBe` True

        -- removing a wrong contact
        hasBeenRemoved <- fromRight' <$> (runAppM env $ removeContact user otherUserId)
        hasBeenRemoved `shouldBe` False

clearContact :: IO ()
clearContact = do
  conn <- connectPostgreSQL testDbConnStr
  execute_ conn "DELETE FROM contacts;" >> return ()

-- | Utility function that can be used in the ``before_`` hook
populateContact :: IO Env -> IO ()
populateContact ioEnv = do
    env <- ioEnv
    uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
    user <- fromRight' <$> (runAppM env $ getUser $ M.emailAddress dummyNewUser)
    let myContact = makeDummyNewUser "first@gmail.com"
    myContactUid <- fromRight' <$> (runAppM env $ newUser myContact)
    hasBeenAdded <- fromRight' <$> (runAppM env $ addContact (fromJust user) myContactUid)
    hasBeenAdded `shouldBe` True

defaultEnv :: IO Env
defaultEnv = do
  conn <- connectPostgreSQL testDbConnStr
  return $ Env Dev conn

  -- describe "insertDWhat tests" $ do
  --   it "insertDWhat test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertDWhen tests" $ do
  --   it "insertDWhen test 1" $ \(conn, env) -> do
  --     1 `shouldBe` 1

  -- describe "insertDWhy tests" $ do
  --   it "insertDWhy test 1" $ \(conn, env) -> do
  -- describe "getPublicKey tests" $ do
  --   it "getPublicKey test 1" $ \(conn, env) -> do
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
