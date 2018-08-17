{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Mirza.SupplyChain.Tests.Service
  ( testServiceQueries
  ) where

import           Mirza.SupplyChain.Tests.Dummies
import           Mirza.SupplyChain.Tests.Settings

import           Mirza.SupplyChain.Auth
import           Mirza.SupplyChain.Database.Schema            as Schema
import           Mirza.SupplyChain.Handlers.Contacts
import           Mirza.SupplyChain.Handlers.EventRegistration
import           Mirza.SupplyChain.Handlers.Queries
import           Mirza.SupplyChain.Handlers.Users
import           Mirza.SupplyChain.Types                      as ST

import           Control.Monad                                (void)
import           Data.Maybe                                   (fromJust)

import           Data.Text.Encoding                           (encodeUtf8)

import           Database.Beam
import           Database.PostgreSQL.Simple                   (connectPostgreSQL,
                                                               execute_)
import           GHC.Stack                                    (HasCallStack)
import           Servant
import           Test.Hspec

import           Text.Email.Validate                          (toByteString)

import qualified Crypto.Scrypt                                as Scrypt

testAppM :: context -> AppM context AppError a -> IO a
testAppM scsContext act = runAppM scsContext act >>= \case
    Left err -> fail (show err)
    Right a -> pure a

testServiceQueries :: HasCallStack => SpecWith SCSContext
testServiceQueries = do

  -- describe "eventSign - INCOMPLETE " $ do
  --   it "Signing an event with a revoked key" $ \scsContext -> do
  --     nowish <- getCurrentTime
  --     let hundredMinutes = 100 * 60
  --         someTimeLater = addUTCTime (hundredMinutes) nowish
  --     pubKey <- rsaPubKey
  --     myKeyState <- testAppM scsContext $ do

  --       -- Adding a user, key, and revoking the key
  --       uid <- newUser dummyNewUser
  --       storageUser <- runDb $ getUserById uid
  --       let user = userTableToModel . fromJust $ storageUser
  --       keyId <- addPublicKey user pubKey (Just . ExpirationTime $ someTimeLater)
  --       _timeKeyRevoked <- revokePublicKey user keyId
  --       keyInfo <- getPublicKeyInfo keyId

  --       -- Adding the event
  --       (_insertedEvent, (Schema.EventId eventId)) <- insertObjectEvent dummyUser dummyObject
  --       let myDigest = SHA256
  --           mySign = Signature "c2FqaWRhbm93ZXIyMw=="
  --           mySignedEvent = SignedEvent (EvId.EventId eventId) keyId mySign myDigest
  --       -- if this test can proceed after the following statement
  --       _eventSignId <- eventSign user mySignedEvent
  --       -- it means the basic functionality of ``eventSign`` function is perhaps done
  --       pure (keyState keyInfo)
  --     myKeyState `shouldBe` InEffect

  describe "newUser tests" $
    it "newUser test 1" $ \scsContext -> do
      res <- testAppM scsContext $  do
        uid <- newUser dummyNewUser
        user <- runDb $ getUserById uid
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        ((ST.UserId uid), Just user :: Maybe (Schema.UserT Identity)) ->
          user `shouldSatisfy`
            (\u ->
              (Schema.user_phone_number u) == (newUserPhoneNumber dummyNewUser) &&
              (Schema.user_email_address u) == (emailToText . newUserEmailAddress $ dummyNewUser) &&
              (Schema.user_first_name u) == (newUserFirstName dummyNewUser) &&
              (Schema.user_last_name u) == (newUserLastName dummyNewUser) &&
              (Schema.user_biz_id u) == (Schema.BizId (newUserCompany dummyNewUser)) &&
              -- note database bytestring includes the salt, this checks password
              (Scrypt.verifyPass'
                (Scrypt.Pass $ encodeUtf8 $ newUserPassword dummyNewUser)
                (Scrypt.EncryptedPass $ Schema.user_password_hash u)) &&
              (Schema.user_id u) == uid
            )

  describe "authCheck tests" $
    it "authCheck test 1" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- newUser dummyNewUser
        let check = unBasicAuthCheck $ authCheck scsContext
        let basicAuthData = BasicAuthData
                          (toByteString $ newUserEmailAddress dummyNewUser)
                          (encodeUtf8   $ newUserPassword     dummyNewUser)

        user <- liftIO $ check basicAuthData
        pure (uid, user)
      case res of
        (uid, Authorized user) ->
          user `shouldSatisfy`
            (\u ->
              (userId u) == uid &&
              (userFirstName u) == (newUserFirstName dummyNewUser) &&
              (userLastName u) == (newUserLastName dummyNewUser)
            )
        _ -> fail "Could not authenticate user"

  describe "Object Event" $ do
    it "Insert Object Event" $ \scsContext -> do
      (insertedEvent, _) <- testAppM scsContext $ insertObjectEvent dummyUser dummyObject
      insertedEvent `shouldBe` dummyObjEvent

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        (insertedEvent, _) <- insertObjectEvent dummyUser dummyObject
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyObjEvent
          evtList `shouldBe` [insertedEvent]

  describe "Aggregation Event" $ do
    it "Insert Aggregation Event" $ \scsContext -> do
      (insertedEvent, _) <- testAppM scsContext $ insertAggEvent dummyUser dummyAggregation
      insertedEvent `shouldBe` dummyAggEvent

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        (insertedEvent, _) <- insertAggEvent dummyUser dummyAggregation
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyAggEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \scsContext -> do
      (insertedEvent, _) <- testAppM scsContext $ insertTransfEvent dummyUser dummyTransformation
      insertedEvent `shouldBe` dummyTransfEvent

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        (insertedEvent, _) <- insertTransfEvent dummyUser dummyTransformation
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (insertedEvent, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyTransfEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transaction Event" $ do
    it "Insert Transaction Event" $ \scsContext -> do
      (insertedEvent, _) <- testAppM scsContext $ insertTransactEvent dummyUser dummyTransaction
      insertedEvent `shouldBe` dummyTransactEvent

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        (insertedEvent, _) <- insertTransactEvent dummyUser dummyTransaction
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
        user <- runDb $ getUser $ newUserEmailAddress dummyNewUser
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        (uid, Just user) ->
          user `shouldSatisfy`
            (\u ->
              (userId u == uid) &&
              (userFirstName u == newUserFirstName dummyNewUser) &&
              (userLastName u == newUserLastName dummyNewUser)
            )

  describe "Contacts" $ do
    (after_ clearContact) . describe "Contacts" $
      describe "Add contact" $
        it "addContact simple" $ \scsContext -> do
          let myContact = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
          (hasBeenAdded, isContact) <- testAppM scsContext $ do
            uid <- newUser dummyNewUser
            user <- runDb $ getUser . newUserEmailAddress $ dummyNewUser
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
          mUser <- runDb $ getUser $ newUserEmailAddress dummyNewUser
          let myContact = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
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
          mUser <- runDb $ getUser $ newUserEmailAddress dummyNewUser
        -- Add a new user who is NOT a contact
          otherUserId <- newUser $ makeDummyNewUser (unsafeMkEmailAddress "other@gmail.com")
          let myContact = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
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
          mUser <- runDb $ getUser $ newUserEmailAddress dummyNewUser
          let myContact = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- newUser myContact
          mMyContact_user <- runDb $ getUser (unsafeMkEmailAddress "first@gmail.com")
          hasBeenAdded <- addContact user myContactUid
          contactList <- listContacts user
          pure (hasBeenAdded, contactList, [fromJust mMyContact_user])
        contactList `shouldBe` users
        hasBeenAdded `shouldBe` True
      it "Add many and list" $ \scsContext -> do
        (contactList, users) <- testAppM scsContext $ do
          -- Making the users
          let myContact_1 = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
              myContact_2 = makeDummyNewUser (unsafeMkEmailAddress "second@gmail.com")
              myContact_3 = makeDummyNewUser (unsafeMkEmailAddress "third@gmail.com")
              myContact_4 = makeDummyNewUser (unsafeMkEmailAddress "fourth@gmail.com")

          -- Adding the users to the DB
          void $ newUser dummyNewUser
          myContactUid_1 <- newUser myContact_1
          myContactUid_2 <- newUser myContact_2
          myContactUid_3 <- newUser myContact_3
          myContactUid_4 <- newUser myContact_4

          -- Getting the users
          mUser <- runDb $ getUser $ newUserEmailAddress dummyNewUser
          mMyContact_1 <- runDb $ getUser (unsafeMkEmailAddress "first@gmail.com")
          mMyContact_2 <- runDb $ getUser (unsafeMkEmailAddress "second@gmail.com")
          mMyContact_3 <- runDb $ getUser (unsafeMkEmailAddress "third@gmail.com")
          mMyContact_4 <- runDb $ getUser (unsafeMkEmailAddress "fourth@gmail.com")

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
      let eventId = Schema.EventId dummyId
      insertedDWhere <- testAppM scsContext $ do
        void $ runDb $ insertDWhere dummyDWhere eventId
        runDb $ findDWhere eventId
      insertedDWhere `shouldBe` Just dummyDWhere

clearContact :: IO ()
clearContact = do
  conn <- connectPostgreSQL testDbConnStr
  void $ execute_ conn "DELETE FROM contacts;"
