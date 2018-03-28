{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.BeamQueries where

import           Test.Hspec

import           BeamQueries
import           Database.Beam
import           Database.PostgreSQL.Simple
import           Dummies
import           QueryUtils

import           Crypto.Scrypt
import           Data.Either.Combinators
import           Data.Maybe                 (fromJust, isNothing)

import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict)
import           Data.GS1.EPC
import           Data.Text.Encoding         (encodeUtf8)

import           AppConfig                  (AppM, Env (..), EnvType (..),
                                             runAppM, runDb)
import           Data.Binary
import qualified Data.Text                  as T
import           Data.Time                  (ZonedTime (..), utcToZonedTime,
                                             zonedTimeToUTC)
import           Data.Time.Clock            (UTCTime (..), getCurrentTime)
import           Data.Time.LocalTime        (LocalTime, utc, utcToLocalTime)
import           Database.PostgreSQL.Simple (execute_)
import           Migrate                    (testDbConnStr)
import qualified Model                      as M
import qualified Service                    as S
import qualified StorageBeam                as SB
import           Utils
-- NOTE in this file, where fromJust is used in the tests, it is because we expect a Just... this is part of the test
-- NOTE tables dropped after every running of test in an "it"

-- for grabbing the encrypted password from user 1
hashIO :: MonadIO m => m ByteString
hashIO = getEncryptedPass <$> (liftIO $ encryptPassIO' (Pass $ encodeUtf8 $ M.password dummyNewUser))

timeStampIO :: MonadIO m => m LocalTime
timeStampIO = liftIO $ (utcToLocalTime utc) <$> getCurrentTime

timeStampIOEPCIS :: MonadIO m => m EPCISTime
timeStampIOEPCIS = liftIO $ getCurrentTime


rsaPubKey :: IO M.RSAPublicKey
rsaPubKey = do
  pemStr <- Prelude.readFile "./test/Tests/testKeys/goodKeys/test.pub"
  return (M.PEMString pemStr)


selectKey :: M.KeyID -> AppM (Maybe SB.Key)
selectKey keyId = do
  r <- runDb $
          runSelectReturningList $ select $ do
          key <- all_ (SB._keys SB.supplyChainDb)
          guard_ (SB.key_id key ==. val_ keyId)
          pure key
  case r of
    Right [key] -> return $ Just key
    _           -> return Nothing

testQueries :: SpecWith (Connection, Env)
testQueries = do

  describe "addPublicKey tests" $ do
    it "addPublicKey test 1" $ \(conn, env) -> do
      pubKey <- rsaPubKey
      tStart <- timeStampIO
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      storageUser <- fromRight' <$> (runAppM env $ selectUser uid)
      let user = userTableToModel . fromJust $ storageUser
      let (M.PEMString keyStr) = pubKey
      keyId <- fromRight' <$> (runAppM env $ S.addPublicKey user pubKey)
      tEnd <- timeStampIO
      keyDB <- fromRight' <$> (runAppM env $ getPublicKey keyId)
      key <- fromRight' <$> (runAppM env $ selectKey keyId)
      (fromJust key)
        `shouldSatisfy`
          (\k ->
            T.unpack (SB.pem_str k) == keyStr &&
            (SB.key_id k) == keyId &&
            (SB.key_user_id k) == (SB.UserId uid) &&
            (SB.creation_time k) > tStart &&
            (SB.creation_time k) < tEnd &&
            isNothing (SB.revocation_time k)
          )
  describe "getPublicKeyInfo tests" $ do
    it "getPublicKeyInfo test 1" $ \(conn, env) -> do
      tStart <- timeStampIOEPCIS
      pubKey <- rsaPubKey
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      storageUser <- fromRight' <$> (runAppM env $ selectUser uid)
      let user = userTableToModel . fromJust $ storageUser
      keyId <- fromRight' <$> (runAppM env $ S.addPublicKey user pubKey)
      keyInfo <- fromRight' <$> (runAppM env $ getPublicKeyInfo keyId)
      tEnd <- timeStampIOEPCIS
      keyInfo
        `shouldSatisfy`
          (\ki ->
            (M.userID ki == uid) &&
            (M.creationTime ki > tStart && M.creationTime ki < tEnd) &&
            isNothing (M.revocationTime ki)
          )

  describe "newUser tests" $ do
    it "newUser test 1" $ \(conn, env) -> do
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      user <- fromRight' <$> (runAppM env $ selectUser uid)

      (fromJust user)
        `shouldSatisfy`
          (\u ->
            (SB.phone_number u) == (M.phoneNumber dummyNewUser) &&
            (SB.email_address u) == (M.emailAddress dummyNewUser) &&
            (SB.first_name u) == (M.firstName dummyNewUser) &&
            (SB.last_name u) == (M.lastName dummyNewUser) &&
            (SB.user_biz_id u) == (SB.BizId (M.company dummyNewUser)) &&
            -- note database bytestring includes the salt, this checks password
            (verifyPass'
              (Pass $ encodeUtf8 $ M.password dummyNewUser)
              (EncryptedPass $ SB.password_hash u)) &&
            (SB.user_id u) == uid
          )

  describe "authCheck tests" $ do
    it "authCheck test 1" $ \(conn, env) -> do
      uid <- fromRight' <$> (runAppM env $ newUser dummyNewUser)
      user <- fromRight' <$> (runAppM env $ authCheck (M.emailAddress dummyNewUser) (encodeUtf8 $ M.password dummyNewUser)) --hash)
      (fromJust user) `shouldSatisfy`
        ( \u ->
          (M.userId u) == uid &&
          (M.userFirstName u) == (M.firstName dummyNewUser) &&
          (M.userLastName u) == (M.lastName dummyNewUser)
        )

  describe "Object Event" $ do
    it "Insert Object Event" $ \(conn, env) -> do
      insertedEvent <- fromRight' <$> (runAppM env $ insertObjectEvent dummyUser dummyObject)
      insertedEvent `shouldSatisfy`
        (\ev -> ev == dummyObjEvent)

    it "List event" $ \(conn, env) -> do
      insertedEvent <- fromRight' <$> (runAppM env $ insertObjectEvent dummyUser dummyObject)
      eventList <- fromRight' <$> (runAppM env $ listEvents dummyLabelEpc)
      insertedEvent `shouldSatisfy`
        (\ev -> ev == dummyObjEvent)
      eventList `shouldBe` [insertedEvent]

  describe "Aggregation Event" $ do
    it "Insert Aggregation Event" $ \(conn, env) -> do
      insertedEvent <- fromRight' <$> (runAppM env $ insertAggEvent dummyUser dummyAggregation)
      insertedEvent `shouldSatisfy`
        (\ev -> ev == dummyAggEvent)

    it "List event" $ \(conn, env) -> do
      insertedEvent <- fromRight' <$> (runAppM env $ insertAggEvent dummyUser dummyAggregation)
      insertedEvent `shouldSatisfy`
        (\ev -> ev == dummyAggEvent)
      eventList <- fromRight' <$> (runAppM env $ listEvents dummyLabelEpc)
      eventList `shouldBe` [insertedEvent]

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \(conn, env) -> do
      insertedEvent <- fromRight' <$> (runAppM env $ insertTransfEvent dummyUser dummyTransformation)
      insertedEvent `shouldSatisfy`
        (\ev -> ev == dummyTransfEvent)

    it "List event" $ \(conn, env) -> do
      insertedEvent <- fromRight' <$> (runAppM env $ insertTransfEvent dummyUser dummyTransformation)
      insertedEvent `shouldSatisfy`
        (\ev -> ev == dummyTransfEvent)
      eventList <- fromRight' <$> (runAppM env $ listEvents dummyLabelEpc)
      eventList `shouldBe` [insertedEvent]

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

  -- TODO Complete this
  describe "DWhere" $ do
    it "Insert DWhere" $ \(conn, env) -> do
      let eventId = dummyId
      insertedDWhere <- fromRight' <$> (runAppM env $ insertDWhere dummyDWhere eventId)
      1 `shouldBe` 1

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
