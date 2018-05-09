{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Tests.Service where

import           Mirza.SupplyChain.Dummies
import           Mirza.SupplyChain.Migrate     (testDbConnStr)
import qualified Mirza.SupplyChain.Model       as M
import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.Service     as S
import qualified Mirza.SupplyChain.StorageBeam as SB

import           Data.GS1.EPC

import           Control.Monad                 (void)
import           Mirza.SupplyChain.AppConfig   (AppError, AppM, DB,
                                                SCSContext (..),
                                                SCSContextType (..), pg,
                                                runAppM, runDb)
-- import           Crypto.Scrypt
import           Data.ByteString               (ByteString)
import           Data.Maybe                    (fromJust, isNothing)
import qualified Data.Text                     as T
import           Data.Text.Encoding            (encodeUtf8)
import           Data.Time.Clock               (getCurrentTime)
import           Data.Time.LocalTime           (LocalTime, utc, utcToLocalTime)
import           Database.Beam
import           Database.PostgreSQL.Simple    (Connection, close,
                                                connectPostgreSQL, execute_)
import           GHC.Stack                     (HasCallStack)
import           Servant
import           Test.Hspec

import qualified Crypto.Scrypt                 as Scrypt
import           Data.Pool                     as Pool
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

rsaPubKey :: IO M.PEM_RSAPubKey
rsaPubKey = M.PEMString <$> Prelude.readFile "./test/Tests/testKeys/goodKeys/test.pub"

selectKey :: M.KeyID -> DB SCSContext AppError (Maybe SB.Key)
selectKey (M.KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
          key <- all_ (SB._keys SB.supplyChainDb)
          guard_ (SB.key_id key ==. val_ keyId)
          pure key
  case r of
    [key] -> return $ Just key
    _     -> return Nothing

testAppM :: SCSContext -> AppM a -> IO a
testAppM env act = runAppM env act >>= \case
    Left err -> fail (show err)
    Right a -> pure a

testQueries :: HasCallStack => SpecWith (Connection, SCSContext)
testQueries = do

  describe "addPublicKey tests" $
    it "addPublicKey test 1" $ \(_conn, env) -> do
      pubKey <- rsaPubKey
      tStart <- timeStampIO
      res <- testAppM env $ do
        uid <- S.newUser dummyNewUser
        storageUser <- runDb $ selectUser uid
        let user = userTableToModel . fromJust $ storageUser
        let (M.PEMString keyStr) = pubKey
        keyId <- S.addPublicKey user pubKey
        tEnd <- timeStampIO
        insertedKey <- S.getPublicKey keyId
        storageKey <- runDb $ selectKey keyId
        pure (storageKey, keyStr, keyId, uid, tEnd, insertedKey)
      case res of
        (Nothing, _, _, _, _, _) -> fail "Received Nothing for key"
        (Just key, keyStr, (M.KeyID keyId), (M.UserID uid), tEnd, insertedKey) -> do
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
        uid <- S.newUser dummyNewUser
        storageUser <- runDb $ selectUser uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- S.addPublicKey user pubKey
        keyInfo <- S.getPublicKeyInfo keyId
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
        uid <- S.newUser dummyNewUser
        user <- runDb $ selectUser uid
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        ((M.UserID uid), Just user :: Maybe (SB.UserT Identity)) ->
          user `shouldSatisfy`
            (\u ->
              (SB.phone_number u) == (M.phoneNumber dummyNewUser) &&
              (SB.email_address u) == (M.unEmailAddress . M.emailAddress $ dummyNewUser) &&
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
        uid <- S.newUser dummyNewUser
        let check = unBasicAuthCheck $ S.authCheck env
        let basicAuthData = BasicAuthData
                          (encodeUtf8 $ M.unEmailAddress $ M.emailAddress dummyNewUser)
                          (encodeUtf8 $ M.password dummyNewUser)

        user <- liftIO $ check basicAuthData
        pure (uid, user)
      case res of
        -- (_, Nothing) -> fail "Received Nothing for user"
        (uid, Authorized user) ->
          user `shouldSatisfy`
            (\u ->
              (M.userId u) == uid &&
              (M.userFirstName u) == (M.firstName dummyNewUser) &&
              (M.userLastName u) == (M.lastName dummyNewUser)
            )
        _ -> fail "Could not authenticate user"

  describe "Object Event" $ do
    it "Insert Object Event" $ \(_conn, env) -> do
      insertedEvent <- testAppM env $ S.insertObjectEvent dummyUser dummyObject
      insertedEvent `shouldBe` dummyObjEvent

    it "List event" $ \(_conn, env) -> do
      res <- testAppM env $ do
        insertedEvent <- S.insertObjectEvent dummyUser dummyObject
        evtList <- S.listEvents dummyUser (M.LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyObjEvent
          evtList `shouldBe` [insertedEvent]

  describe "Aggregation Event" $ do
    it "Insert Aggregation Event" $ \(_conn, env) -> do
      insertedEvent <- testAppM env $ S.insertAggEvent dummyUser dummyAggregation
      insertedEvent `shouldBe` dummyAggEvent

    it "List event" $ \(_conn, env) -> do
      res <- testAppM env $ do
        insertedEvent <- S.insertAggEvent dummyUser dummyAggregation
        evtList <- S.listEvents dummyUser (M.LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyAggEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \(_conn, env) -> do
      insertedEvent <- testAppM env $ S.insertTransfEvent dummyUser dummyTransformation
      insertedEvent `shouldBe` dummyTransfEvent

    it "List event" $ \(_conn, env) -> do
      res <- testAppM env $ do
        insertedEvent <- S.insertTransfEvent dummyUser dummyTransformation
        eventList <- S.listEvents dummyUser (M.LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, eventList)
      case res of
        (insertedEvent, eventList) -> do
          insertedEvent `shouldBe` dummyTransfEvent
          eventList `shouldBe` [insertedEvent]

  describe "Transaction Event" $ do
    it "Insert Transaction Event" $ \(_conn, env) -> do
      insertedEvent <- testAppM env $ S.insertTransactEvent dummyUser dummyTransaction
      insertedEvent `shouldBe` dummyTransactEvent

    it "List event" $ \(_conn, env) -> do
      res <- testAppM env $ do
        insertedEvent <- S.insertTransactEvent dummyUser dummyTransaction
        eventList <- S.listEvents dummyUser (M.LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, eventList)
      case res of
        (insertedEvent, eventList) -> do
          insertedEvent `shouldBe` dummyTransactEvent
          eventList `shouldBe` [insertedEvent]

  describe "runDb $ getUser tests" $
    it "runDb $ getUser test 1" $ \(_conn, env) -> do
      res <- testAppM env $ do
        uid <- S.newUser dummyNewUser
        user <- runDb $ getUser $ M.emailAddress dummyNewUser
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

  describe "Contacts" $ do
    (after_ clearContact) . describe "Contacts" $
      describe "Add contact" $
        it "addContact simple" $ \(_conn, env) -> do
          let myContact = makeDummyNewUser (M.EmailAddress "first@gmail.com")
          (hasBeenAdded, isContact) <- testAppM env $ do
            uid <- S.newUser dummyNewUser
            user <- runDb $ getUser . M.emailAddress $ dummyNewUser
            myContactUid <- S.newUser myContact
            hasBeenAdded <- S.addContact (fromJust user) myContactUid
            isContact <- runDb $ isExistingContact uid myContactUid
            pure (hasBeenAdded, isContact)
          hasBeenAdded `shouldBe` True
          isContact `shouldBe` True

    describe "Remove contact" $ do
      it "Simple remove one" $ \(_conn, env) -> do
        -- Adding the contact first
        (hasBeenAdded, hasBeenRemoved) <- testAppM env $ do
          void $ S.newUser dummyNewUser
          mUser <- runDb $ getUser $ M.emailAddress dummyNewUser
          let myContact = makeDummyNewUser (M.EmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- S.newUser myContact
          hasBeenAdded <- S.addContact user myContactUid
        -- removing the contact now
          hasBeenRemoved <- S.removeContact user myContactUid
          pure (hasBeenAdded, hasBeenRemoved)
        hasBeenAdded `shouldBe` True
        hasBeenRemoved `shouldBe` True
      it "Remove wrong contact" $ \(_conn, env) -> do
        -- Adding the contact first
        (hasBeenAdded, hasBeenRemoved) <- testAppM env $ do
          void $ S.newUser dummyNewUser
          mUser <- runDb $ getUser $ M.emailAddress dummyNewUser
        -- Add a new user who is NOT a contact
          otherUserId <- S.newUser $ makeDummyNewUser (M.EmailAddress "other@gmail.com")
          let myContact = makeDummyNewUser (M.EmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- S.newUser myContact
          hasBeenAdded <- S.addContact user myContactUid
        -- removing a wrong contact
          hasBeenRemoved <- S.removeContact user otherUserId
          pure (hasBeenAdded, hasBeenRemoved)
        hasBeenAdded `shouldBe` True
        hasBeenRemoved `shouldBe` False

    describe "List contact" $ do
      it "Add one and list" $ \(_conn, env) -> do
        -- Adding the contact first
        (hasBeenAdded, contactList, users) <- testAppM env $ do
          void $ S.newUser dummyNewUser
          mUser <- runDb $ getUser $ M.emailAddress dummyNewUser
          let myContact = makeDummyNewUser (M.EmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- S.newUser myContact
          mMyContact_user <- runDb $ getUser (M.EmailAddress "first@gmail.com")
          hasBeenAdded <- S.addContact user myContactUid
          contactList <- S.listContacts user
          pure (hasBeenAdded, contactList, [fromJust mMyContact_user])
        contactList `shouldBe` users
        hasBeenAdded `shouldBe` True
      it "Add many and list" $ \(_conn, env) -> do
        (contactList, users) <- testAppM env $ do
          -- Making the users
          let myContact_1 = makeDummyNewUser (M.EmailAddress "first@gmail.com")
              myContact_2 = makeDummyNewUser (M.EmailAddress "second@gmail.com")
              myContact_3 = makeDummyNewUser (M.EmailAddress "third@gmail.com")
              myContact_4 = makeDummyNewUser (M.EmailAddress "fourth@gmail.com")

          -- Adding the users to the DB
          void $ S.newUser dummyNewUser
          myContactUid_1 <- S.newUser myContact_1
          myContactUid_2 <- S.newUser myContact_2
          myContactUid_3 <- S.newUser myContact_3
          myContactUid_4 <- S.newUser myContact_4

          -- Getting the users
          mUser <- runDb $ getUser $ M.emailAddress dummyNewUser
          mMyContact_1 <- runDb $ getUser (M.EmailAddress "first@gmail.com")
          mMyContact_2 <- runDb $ getUser (M.EmailAddress "second@gmail.com")
          mMyContact_3 <- runDb $ getUser (M.EmailAddress "third@gmail.com")
          mMyContact_4 <- runDb $ getUser (M.EmailAddress "fourth@gmail.com")

          let user = fromJust mUser
              myContact_1_user = fromJust mMyContact_1
              myContact_2_user = fromJust mMyContact_2
              myContact_3_user = fromJust mMyContact_3
              myContact_4_user = fromJust mMyContact_4

          -- Making them contacts
          void $ S.addContact user myContactUid_1
          void $ S.addContact user myContactUid_2
          void $ S.addContact user myContactUid_3
          -- intentionally not adding contact_4

          -- Randomness
          void $ S.addContact myContact_3_user myContactUid_4
          void $ S.addContact myContact_3_user myContactUid_2
          void $ S.addContact myContact_4_user myContactUid_2
          void $ S.addContact myContact_1_user myContactUid_2

          contactList <- S.listContacts user
          pure (contactList, [myContact_1_user, myContact_2_user, myContact_3_user])
        contactList `shouldBe` users

  describe "DWhere" $
    it "Insert and find DWhere" $ \(_conn, env) -> do
      let eventId = dummyId
      insertedDWhere <- testAppM env $ do
        void $ runDb $ insertDWhere dummyDWhere eventId
        runDb $ findDWhere eventId
      insertedDWhere `shouldBe` Just dummyDWhere

clearContact :: IO ()
clearContact = do
  conn <- connectPostgreSQL testDbConnStr
  void $ execute_ conn "DELETE FROM contacts;"

-- | Utility function that can be used in the ``before_`` hook
populateContact :: IO SCSContext -> IO ()
populateContact ioSCSContext = do
    scsContext <- ioSCSContext
    let myContact = makeDummyNewUser (M.EmailAddress "first@gmail.com")
    hasBeenAdded <- testAppM scsContext $ do
      void $ S.newUser dummyNewUser
      user <- runDb $ getUser $ M.emailAddress dummyNewUser
      myContactUid <- S.newUser myContact
      S.addContact (fromJust user) myContactUid
    hasBeenAdded `shouldBe` True

defaultSCSContext :: IO SCSContext
defaultSCSContext = (\conn -> SCSContext Dev conn Scrypt.defaultParams) <$> defaultPool

defaultPool :: IO (Pool Connection)
defaultPool = Pool.createPool (connectPostgreSQL testDbConnStr) close
                1 -- Number of "sub-pools",
                60 -- How long in seconds to keep a connection open for reuse
                10 -- Max number of connections to have open at any one time

