{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Tests.BeamQueries where

import           AppConfig                  (AppM, Env (..), EnvType (..),
                                             runAppM, runDb)
import           BeamQueries
import           Control.Monad              (void)
-- import           Crypto.Scrypt
import           Data.ByteString            (ByteString)
import           Data.GS1.EPC
import           Data.Maybe                 (fromJust, isNothing)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.LocalTime        (LocalTime, utc, utcToLocalTime)
import           Database.Beam
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL,
                                             execute_)
import           Dummies
import           GHC.Stack                  (HasCallStack)
import           Migrate                    (testDbConnStr)
import qualified Model                      as M
import           QueryUtils
import qualified Service                    as S
import qualified StorageBeam                as SB
import           Test.Hspec

import qualified Crypto.Scrypt              as Scrypt

-- NOTE in this file, where fromJust is used in the tests, it is because we expect a Just... this is part of the test
-- NOTE tables dropped after every running of test in an "it"

-- for grabbing the encrypted password from user 1
hashIO :: MonadIO m => m ByteString
hashIO = Scrypt.getEncryptedPass <$> liftIO
    (Scrypt.encryptPassIO' (Scrypt.Pass $ encodeUtf8 $ M.password dummyNewUser))

timeStampIO :: MonadIO m => m LocalTime
timeStampIO = liftIO $ (utcToLocalTime utc) <$> getCurrentTime

timeStampIOEPCIS :: MonadIO m => m EPCISTime
timeStampIOEPCIS = liftIO $ EPCISTime <$> getCurrentTime


rsaPubKey :: IO M.RSAPublicKey
rsaPubKey = M.PEMString <$> Prelude.readFile "./test/Tests/testKeys/goodKeys/test.pub"


selectKey :: M.KeyID -> AppM (Maybe SB.Key)
selectKey keyId = do
  r <- runDb $
          runSelectReturningList $ select $ do
          key <- all_ (SB._keys SB.supplyChainDb)
          guard_ (SB.key_id key ==. val_ keyId)
          pure key
  case r of
    [key] -> return $ Just key
    _     -> return Nothing

testAppM :: Env -> AppM a -> IO a
testAppM env act = runAppM env act >>= \case
    Left err -> fail (show err)
    Right a -> pure a

testQueries :: HasCallStack => SpecWith (Connection, Env)
testQueries = do

  describe "addPublicKey tests" $
    it "addPublicKey test 1" $ \(_conn, env) -> do
      pubKey <- rsaPubKey
      tStart <- timeStampIO
      res <- testAppM env $ do
        uid <- newUser dummyNewUser
        storageUser <- selectUser uid
        let user = userTableToModel . fromJust $ storageUser
        let (M.PEMString keyStr) = pubKey
        keyId <- S.addPublicKey user pubKey
        tEnd <- timeStampIO
        insertedKey <- getPublicKey keyId
        storageKey <- selectKey keyId
        pure (storageKey, keyStr, keyId, uid, tEnd, insertedKey)
      case res of
        (Nothing, _, _, _, _, _) -> fail "Received Nothing for key"
        (Just key, keyStr, keyId, uid, tEnd, insertedKey) -> do
          key `shouldSatisfy`
            (\k ->
              T.unpack (SB.pem_str k) == keyStr &&
              (SB.key_id k) == keyId &&
              (SB.key_user_id k) == (SB.UserId uid) &&
              (SB.creation_time k) > tStart &&
              (SB.creation_time k) < tEnd &&
              isNothing (SB.revocation_time k)
            )
          insertedKey `shouldBe` pubKey
  describe "getPublicKeyInfo tests" $
    it "getPublicKeyInfo test 1" $ \(_conn, env) -> do
      tStart <- timeStampIOEPCIS
      pubKey <- rsaPubKey
      (keyInfo, uid, tEnd) <- testAppM env $ do
        uid <- newUser dummyNewUser
        storageUser <- selectUser uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- S.addPublicKey user pubKey
        keyInfo <- getPublicKeyInfo keyId
        tEnd <- timeStampIOEPCIS
        pure (keyInfo, uid, tEnd)
      keyInfo `shouldSatisfy`
        (\ki ->
          (M.userID ki == uid) &&
          (M.creationTime ki > tStart && M.creationTime ki < tEnd) &&
          isNothing (M.revocationTime ki)
        )

  describe "newUser tests" $
    it "newUser test 1" $ \(_conn, env) -> do
      res <- testAppM env $  do
        uid <- newUser dummyNewUser
        user <- selectUser uid
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        (uid, Just user :: Maybe (SB.UserT Identity)) ->
          user `shouldSatisfy`
            (\u ->
              (SB.phone_number u) == (M.phoneNumber dummyNewUser) &&
              (SB.email_address u) == (M.emailAddress dummyNewUser) &&
              (SB.first_name u) == (M.firstName dummyNewUser) &&
              (SB.last_name u) == (M.lastName dummyNewUser) &&
              (SB.user_biz_id u) == (SB.BizId (M.company dummyNewUser)) &&
              -- note database bytestring includes the salt, this checks password
              (Scrypt.verifyPass'
                (Scrypt.Pass $ encodeUtf8 $ M.password dummyNewUser)
                (Scrypt.EncryptedPass $ SB.password_hash u)) &&
              (SB.user_id u) == uid
            )

  describe "authCheck tests" $
    it "authCheck test 1" $ \(_conn, env) -> do
      res <- testAppM env $ do
        uid <- newUser dummyNewUser
        user <- authCheck (M.emailAddress dummyNewUser) (encodeUtf8 $ M.password dummyNewUser)
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        (uid, Just user :: Maybe M.User) ->
          user `shouldSatisfy`
            (\u ->
              (M.userId u) == uid &&
              (M.userFirstName u) == (M.firstName dummyNewUser) &&
              (M.userLastName u) == (M.lastName dummyNewUser)
            )

  describe "Object Event" $ do
    it "Insert Object Event" $ \(_conn, env) -> do
      insertedEvent <- testAppM env $ insertObjectEvent dummyUser dummyObject
      insertedEvent `shouldSatisfy`
        ((== dummyObjEvent))

    it "List event" $ \(_conn, env) -> do
      res <- testAppM env $ do
        insertedEvent <- insertObjectEvent dummyUser dummyObject
        eventList <- listEvents dummyLabelEpc
        pure (insertedEvent, eventList)
      case res of
        (insertedEvent, eventList) -> do
          insertedEvent `shouldSatisfy`
            ((== dummyObjEvent))
          eventList `shouldBe` [insertedEvent]

  describe "Aggregation Event" $ do
    it "Insert Aggregation Event" $ \(_conn, env) -> do
      insertedEvent <- testAppM env $ insertAggEvent dummyUser dummyAggregation
      insertedEvent `shouldSatisfy`
        ((== dummyAggEvent))

    it "List event" $ \(_conn, env) -> do
      res <- testAppM env $ do
        insertedEvent <- insertAggEvent dummyUser dummyAggregation
        eventList <- listEvents dummyLabelEpc
        pure (insertedEvent, eventList)
      case res of
        (insertedEvent, eventList) -> do
          insertedEvent `shouldSatisfy`
            ((== dummyAggEvent))
          eventList `shouldBe` [insertedEvent]

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \(_conn, env) -> do
      insertedEvent <- testAppM env $ insertTransfEvent dummyUser dummyTransformation
      insertedEvent `shouldSatisfy`
        ((== dummyTransfEvent))

    it "List event" $ \(_conn, env) -> do
      res <- testAppM env $ do
        insertedEvent <- insertTransfEvent dummyUser dummyTransformation
        eventList <- listEvents dummyLabelEpc
        pure (insertedEvent, eventList)
      case res of
        (insertedEvent, eventList) -> do
          insertedEvent `shouldSatisfy`
            ((== dummyTransfEvent))
          eventList `shouldBe` [insertedEvent]

  describe "Transaction Event" $ do
    it "Insert Transaction Event" $ \(_conn, env) -> do
      insertedEvent <- testAppM env $ insertTransactEvent dummyUser dummyTransaction
      insertedEvent `shouldSatisfy`
        ((== dummyTransactEvent))

    it "List event" $ \(_conn, env) -> do
      res <- testAppM env $ do
        insertedEvent <- insertTransactEvent dummyUser dummyTransaction
        eventList <- listEvents dummyLabelEpc
        pure (insertedEvent, eventList)
      case res of
        (insertedEvent, eventList) -> do
          insertedEvent `shouldSatisfy`
            ((== dummyTransactEvent))
          eventList `shouldBe` [insertedEvent]

  describe "getUser tests" $
    it "getUser test 1" $ \(_conn, env) -> do
      res <- testAppM env $ do
        uid <- newUser dummyNewUser
        user <- getUser $ M.emailAddress dummyNewUser
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        (uid, Just user) ->
          user `shouldSatisfy`
            (\u ->
              (M.userId u == uid) &&
              (M.userFirstName u == M.firstName dummyNewUser) &&
              (M.userLastName u == M.lastName dummyNewUser)
            )

  (after_ clearContact) . describe "Contacts" $ do
    describe "Add contact" $
      it "addContact simple" $ \(_conn, env) -> do
        let myContact = makeDummyNewUser "first@gmail.com"
        (hasBeenAdded, isContact) <- testAppM env $ do
          uid <- newUser dummyNewUser
          user <- getUser . M.emailAddress $ dummyNewUser
          myContactUid <- newUser myContact
          hasBeenAdded <- addContact (fromJust user) myContactUid
          isContact <- isExistingContact uid myContactUid
          pure (hasBeenAdded, isContact)
        hasBeenAdded `shouldBe` True
        isContact `shouldBe` True

    describe "Remove contact" $ do
      it "Simple remove one" $ \(_conn, env) -> do
        -- Adding the contact first
        (hasBeenAdded, hasBeenRemoved) <- testAppM env $ do
          void $ newUser dummyNewUser
          mUser <- getUser $ M.emailAddress dummyNewUser
          let myContact = makeDummyNewUser "first@gmail.com"
              user = fromJust mUser
          myContactUid <- newUser myContact
          hasBeenAdded <- addContact user myContactUid
        -- removing the contact now
          hasBeenRemoved <- removeContact user myContactUid
          pure (hasBeenAdded, hasBeenRemoved)
        hasBeenAdded `shouldBe` True
        hasBeenRemoved `shouldBe` True
      it "Remove wrong contact" $ \(_conn, env) -> do
        -- Adding the contact first
        (hasBeenAdded, hasBeenRemoved) <- testAppM env $ do
          void $ newUser dummyNewUser
          mUser <- getUser $ M.emailAddress dummyNewUser
        -- Add a new user who is NOT a contact
          otherUserId <- newUser $ makeDummyNewUser "other@gmail.com"
          let myContact = makeDummyNewUser "first@gmail.com"
              user = fromJust mUser
          myContactUid <- newUser myContact
          hasBeenAdded <- addContact user myContactUid
        -- removing a wrong contact
          hasBeenRemoved <- removeContact user otherUserId
          pure (hasBeenAdded, hasBeenRemoved)
        hasBeenAdded `shouldBe` True
        hasBeenRemoved `shouldBe` False

  describe "DWhere" $
    it "Insert and find DWhere" $ \(_conn, env) -> do
      let eventId = dummyId
      insertedDWhere <- testAppM env $ do
        void $ insertDWhere dummyDWhere eventId
        findDWhere eventId
      insertedDWhere `shouldBe` Just dummyDWhere

clearContact :: IO ()
clearContact = do
  conn <- connectPostgreSQL testDbConnStr
  void $ execute_ conn "DELETE FROM contacts;"

-- | Utility function that can be used in the ``before_`` hook
populateContact :: IO Env -> IO ()
populateContact ioEnv = do
    env <- ioEnv
    let myContact = makeDummyNewUser "first@gmail.com"
    hasBeenAdded <- testAppM env $ do
      void $ newUser dummyNewUser
      user <- getUser $ M.emailAddress dummyNewUser
      myContactUid <- newUser myContact
      hasBeenAdded <- addContact (fromJust user) myContactUid
      pure hasBeenAdded
    hasBeenAdded `shouldBe` True

defaultEnv :: IO Env
defaultEnv = (\conn -> Env Dev conn Scrypt.defaultParams) <$> connectPostgreSQL testDbConnStr
