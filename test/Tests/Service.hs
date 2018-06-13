{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Tests.Service
  ( testQueries
  ) where

import           Tests.Dummies

import           Mirza.SupplyChain.Auth
import           Mirza.SupplyChain.Handlers.Business
import           Mirza.SupplyChain.Handlers.Contacts
import           Mirza.SupplyChain.Handlers.EventRegistration
import           Mirza.SupplyChain.Handlers.Queries
import           Mirza.SupplyChain.Handlers.Users
import           Mirza.SupplyChain.Migrate                    (testDbConnStr)
import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.StorageBeam                as SB
import           Mirza.SupplyChain.Types

import           Control.Monad                                (void)
import           Data.Maybe                                   (fromJust,
                                                               isNothing)
import qualified Data.Text                                    as T
import           Data.Text.Encoding                           (encodeUtf8)
import           Data.Time.Clock                              (getCurrentTime)
import           Data.Time.LocalTime                          (LocalTime, utc,
                                                               utcToLocalTime)
import           Database.Beam
import           Database.PostgreSQL.Simple                   (connectPostgreSQL,
                                                               execute_)
import           GHC.Stack                                    (HasCallStack)
import           Servant
import           Test.Hspec

import qualified Crypto.Scrypt                                as Scrypt
-- NOTE in this file, where fromJust is used in the tests, it is because we expect a Just... this is part of the test
-- NOTE tables dropped after every running of test in an "it"

timeStampIO :: MonadIO m => m LocalTime
timeStampIO = liftIO $ (utcToLocalTime utc) <$> getCurrentTime

rsaPubKey :: IO PEM_RSAPubKey
rsaPubKey = PEMString <$> Prelude.readFile "./test/Tests/testKeys/goodKeys/test.pub"

testAppM :: context -> AppM context AppError a -> IO a
testAppM scsContext act = runAppM scsContext act >>= \case
    Left err -> fail (show err)
    Right a -> pure a

testQueries :: HasCallStack => SpecWith SCSContext
testQueries = do

  describe "addPublicKey tests" $
    it "addPublicKey test 1" $ \scsContext -> do
      pubKey <- rsaPubKey
      tStart <- timeStampIO
      res <- testAppM scsContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        let (PEMString keyStr) = pubKey
        keyId <- addPublicKey user pubKey Nothing
        tEnd <- timeStampIO
        insertedKey <- getPublicKey keyId
        storageKey <- runDb $ getKeyById keyId
        pure (storageKey, keyStr, keyId, uid, tEnd, insertedKey)
      case res of
        (Nothing, _, _, _, _, _) -> fail "Received Nothing for key"
        (Just key, keyStr, (KeyID keyId), (UserID uid), tEnd, insertedKey) -> do
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
    it "getPublicKeyInfo test 1" $ \scsContext -> do
      tStart <- liftIO getCurrentTime
      pubKey <- rsaPubKey
      (keyInfo, uid, tEnd) <- testAppM scsContext $ do
        uid <- newUser dummyNewUser
        storageUser <- runDb $ getUserById uid
        let user = userTableToModel . fromJust $ storageUser
        keyId <- addPublicKey user pubKey Nothing
        keyInfo <- getPublicKeyInfo keyId
        tEnd <- liftIO getCurrentTime
        pure (keyInfo, uid, tEnd)
      keyInfo `shouldSatisfy`
        (\ki ->
          (keyInfoUserId ki == uid) &&
          ((unCreationTime . creationTime $ ki) > tStart &&
           (unCreationTime . creationTime $ ki) < tEnd) &&
          isNothing (revocationTime ki)
        )

  describe "newUser tests" $
    it "newUser test 1" $ \scsContext -> do
      res <- testAppM scsContext $  do
        uid <- newUser dummyNewUser
        user <- runDb $ getUserById uid
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        ((UserID uid), Just user :: Maybe (SB.UserT Identity)) ->
          user `shouldSatisfy`
            (\u ->
              (SB.phone_number u) == (phoneNumber dummyNewUser) &&
              (SB.email_address u) == (unEmailAddress . emailAddress $ dummyNewUser) &&
              (SB.first_name u) == (firstName dummyNewUser) &&
              (SB.last_name u) == (lastName dummyNewUser) &&
              (SB.user_biz_id u) == (SB.BizId (company dummyNewUser)) &&
              -- note database bytestring includes the salt, this checks password
              (Scrypt.verifyPass'
                (Scrypt.Pass $ encodeUtf8 $ password dummyNewUser)
                (Scrypt.EncryptedPass $ SB.password_hash u)) &&
              (SB.user_id u) == uid
            )

  describe "authCheck tests" $
    it "authCheck test 1" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- newUser dummyNewUser
        let check = unBasicAuthCheck $ authCheck scsContext
        let basicAuthData = BasicAuthData
                          (encodeUtf8 $ unEmailAddress $ emailAddress dummyNewUser)
                          (encodeUtf8 $ password dummyNewUser)

        user <- liftIO $ check basicAuthData
        pure (uid, user)
      case res of
        -- (_, Nothing) -> fail "Received Nothing for user"
        (uid, Authorized user) ->
          user `shouldSatisfy`
            (\u ->
              (userId u) == uid &&
              (userFirstName u) == (firstName dummyNewUser) &&
              (userLastName u) == (lastName dummyNewUser)
            )
        _ -> fail "Could not authenticate user"

  describe "Object Event" $ do
    it "Insert Object Event" $ \scsContext -> do
      insertedEvent <- testAppM scsContext $ insertObjectEvent dummyUser dummyObject
      insertedEvent `shouldBe` dummyObjEvent

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        insertedEvent <- insertObjectEvent dummyUser dummyObject
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyObjEvent
          evtList `shouldBe` [insertedEvent]

  describe "Aggregation Event" $ do
    it "Insert Aggregation Event" $ \scsContext -> do
      insertedEvent <- testAppM scsContext $ insertAggEvent dummyUser dummyAggregation
      insertedEvent `shouldBe` dummyAggEvent

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        insertedEvent <- insertAggEvent dummyUser dummyAggregation
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyAggEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \scsContext -> do
      insertedEvent <- testAppM scsContext $ insertTransfEvent dummyUser dummyTransformation
      insertedEvent `shouldBe` dummyTransfEvent

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        insertedEvent <- insertTransfEvent dummyUser dummyTransformation
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyTransfEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transaction Event" $ do
    it "Insert Transaction Event" $ \scsContext -> do
      insertedEvent <- testAppM scsContext $ insertTransactEvent dummyUser dummyTransaction
      insertedEvent `shouldBe` dummyTransactEvent

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        insertedEvent <- insertTransactEvent dummyUser dummyTransaction
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyTransactEvent
          evtList `shouldBe` [insertedEvent]

  describe "getUser tests" $
    it "getUser test 1" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- newUser dummyNewUser
        user <- runDb $ getUser $ emailAddress dummyNewUser
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        (uid, Just user) ->
          user `shouldSatisfy`
            (\u ->
              (userId u == uid) &&
              (userFirstName u == firstName dummyNewUser) &&
              (userLastName u == lastName dummyNewUser)
            )

  describe "Contacts" $ do
    (after_ clearContact) . describe "Contacts" $
      describe "Add contact" $
        it "addContact simple" $ \scsContext -> do
          let myContact = makeDummyNewUser (EmailAddress "first@gmail.com")
          (hasBeenAdded, isContact) <- testAppM scsContext $ do
            uid <- newUser dummyNewUser
            user <- runDb $ getUser . emailAddress $ dummyNewUser
            myContactUid <- newUser myContact
            hasBeenAdded <- addContact (fromJust user) myContactUid
            isContact <- runDb $ isExistingContact uid myContactUid
            pure (hasBeenAdded, isContact)
          hasBeenAdded `shouldBe` True
          isContact `shouldBe` True

    describe "Remove contact" $ do
      it "Simple remove one" $ \scsContext -> do
        -- Adding the contact first
        (hasBeenAdded, hasBeenRemoved) <- testAppM scsContext $ do
          void $ newUser dummyNewUser
          mUser <- runDb $ getUser $ emailAddress dummyNewUser
          let myContact = makeDummyNewUser (EmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- newUser myContact
          hasBeenAdded <- addContact user myContactUid
        -- removing the contact now
          hasBeenRemoved <- removeContact user myContactUid
          pure (hasBeenAdded, hasBeenRemoved)
        hasBeenAdded `shouldBe` True
        hasBeenRemoved `shouldBe` True
      it "Remove wrong contact" $ \scsContext -> do
        -- Adding the contact first
        (hasBeenAdded, hasBeenRemoved) <- testAppM scsContext $ do
          void $ newUser dummyNewUser
          mUser <- runDb $ getUser $ emailAddress dummyNewUser
        -- Add a new user who is NOT a contact
          otherUserId <- newUser $ makeDummyNewUser (EmailAddress "other@gmail.com")
          let myContact = makeDummyNewUser (EmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- newUser myContact
          hasBeenAdded <- addContact user myContactUid
        -- removing a wrong contact
          hasBeenRemoved <- removeContact user otherUserId
          pure (hasBeenAdded, hasBeenRemoved)
        hasBeenAdded `shouldBe` True
        hasBeenRemoved `shouldBe` False

    describe "List contact" $ do
      it "Add one and list" $ \scsContext -> do
        -- Adding the contact first
        (hasBeenAdded, contactList, users) <- testAppM scsContext $ do
          void $ newUser dummyNewUser
          mUser <- runDb $ getUser $ emailAddress dummyNewUser
          let myContact = makeDummyNewUser (EmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- newUser myContact
          mMyContact_user <- runDb $ getUser (EmailAddress "first@gmail.com")
          hasBeenAdded <- addContact user myContactUid
          contactList <- listContacts user
          pure (hasBeenAdded, contactList, [fromJust mMyContact_user])
        contactList `shouldBe` users
        hasBeenAdded `shouldBe` True
      it "Add many and list" $ \scsContext -> do
        (contactList, users) <- testAppM scsContext $ do
          -- Making the users
          let myContact_1 = makeDummyNewUser (EmailAddress "first@gmail.com")
              myContact_2 = makeDummyNewUser (EmailAddress "second@gmail.com")
              myContact_3 = makeDummyNewUser (EmailAddress "third@gmail.com")
              myContact_4 = makeDummyNewUser (EmailAddress "fourth@gmail.com")

          -- Adding the users to the DB
          void $ newUser dummyNewUser
          myContactUid_1 <- newUser myContact_1
          myContactUid_2 <- newUser myContact_2
          myContactUid_3 <- newUser myContact_3
          myContactUid_4 <- newUser myContact_4

          -- Getting the users
          mUser <- runDb $ getUser $ emailAddress dummyNewUser
          mMyContact_1 <- runDb $ getUser (EmailAddress "first@gmail.com")
          mMyContact_2 <- runDb $ getUser (EmailAddress "second@gmail.com")
          mMyContact_3 <- runDb $ getUser (EmailAddress "third@gmail.com")
          mMyContact_4 <- runDb $ getUser (EmailAddress "fourth@gmail.com")

          let user = fromJust mUser
              myContact_1_user = fromJust mMyContact_1
              myContact_2_user = fromJust mMyContact_2
              myContact_3_user = fromJust mMyContact_3
              myContact_4_user = fromJust mMyContact_4

          -- Making them contacts
          void $ addContact user myContactUid_1
          void $ addContact user myContactUid_2
          void $ addContact user myContactUid_3
          -- intentionally not adding contact_4

          -- Randomness
          void $ addContact myContact_3_user myContactUid_4
          void $ addContact myContact_3_user myContactUid_2
          void $ addContact myContact_4_user myContactUid_2
          void $ addContact myContact_1_user myContactUid_2

          contactList <- listContacts user
          pure (contactList, [myContact_1_user, myContact_2_user, myContact_3_user])
        contactList `shouldBe` users

  describe "DWhere" $
    it "Insert and find DWhere" $ \scsContext -> do
      let eventId = dummyId
      insertedDWhere <- testAppM scsContext $ do
        void $ runDb $ insertDWhere dummyDWhere eventId
        runDb $ findDWhere eventId
      insertedDWhere `shouldBe` Just dummyDWhere

clearContact :: IO ()
clearContact = do
  conn <- connectPostgreSQL testDbConnStr
  void $ execute_ conn "DELETE FROM contacts;"


