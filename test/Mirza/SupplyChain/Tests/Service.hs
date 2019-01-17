{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Mirza.SupplyChain.Tests.Service
  ( testServiceQueries
  ) where

import           Mirza.Common.GS1BeamOrphans                  (LabelEPCUrn (..))

import           Mirza.Common.Tests.InitClient                (testDbConnectionStringSCS)
import           Mirza.Common.Tests.Utils                     (getDatabaseConnectionString,
                                                               unsafeMkEmailAddress)
import           Mirza.SupplyChain.Tests.Dummies

import           Mirza.SupplyChain.Auth
import           Mirza.SupplyChain.Database.Schema            as Schema
import           Mirza.SupplyChain.EventUtils
import           Mirza.SupplyChain.Handlers.Contacts
import           Mirza.SupplyChain.Handlers.EventRegistration
import           Mirza.SupplyChain.Handlers.Queries
import           Mirza.SupplyChain.Handlers.Users
import           Mirza.SupplyChain.Types                      as ST

import           Control.Monad                                (void)
import           Control.Monad.Except                         (catchError)

import           Data.GS1.EPC                                 (GS1CompanyPrefix (..))

import           Data.Either                                  (isLeft)
import           Data.List.NonEmpty                           (NonEmpty (..))
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

  describe "addUser tests" $ do
    it "addUser test all valid" $ \scsContext -> do
      res <- testAppM scsContext $  do
        uid <- addUser dummyNewUser
        user <- runDb $ getUserById uid
        pure (uid, user)
      case res of
        (_, Nothing) -> fail "Received Nothing for user"
        ((ST.UserId uid), Just user :: Maybe (Schema.UserT Identity)) ->
          user `shouldSatisfy`
            (\u ->
              (Schema.user_phone_number u) == (newUserPhoneNumber dummyNewUser) &&
              (Schema.user_email_address u) == (newUserEmailAddress dummyNewUser) &&
              (Schema.user_first_name u) == (newUserFirstName dummyNewUser) &&
              (Schema.user_last_name u) == (newUserLastName dummyNewUser) &&
              (Schema.user_biz_id u) == (Schema.BizId (newUserCompany dummyNewUser)) &&
              -- note database bytestring includes the salt, this checks password
              (Scrypt.verifyPass'
                (Scrypt.Pass $ encodeUtf8 $ newUserPassword dummyNewUser)
                (Scrypt.EncryptedPass $ Schema.user_password_hash u)) &&
              (Schema.user_id u) == uid
            )
    it "addUser test users with duplicate emails" $ \scsContext -> do
      testAppM scsContext $  do
        _ <- addUser dummyNewUser
        catchError (addUser dummyNewUser *> pure ()) $ \err -> case err of
          AppError (EmailExists _) -> pure ()
          _                        -> throwError err
  describe "authCheck tests" $
    it "authCheck test 1" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
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
      (evInfo, _) <- testAppM scsContext $ insertObjectEvent dummyUser dummyObject
      (eventInfoEvent evInfo) `shouldBe` dummyObjEvent

    it "Should not allow duplicate Object events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertObjectEvent dummyUser dummyObject
      res <- runAppM @_ @AppError scsContext $ insertObjectEvent dummyUser dummyObject
      res `shouldSatisfy` isLeft

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        (evInfo, _) <- insertObjectEvent dummyUser dummyObject
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (eventInfoEvent evInfo, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyObjEvent
          evtList `shouldBe` [insertedEvent]

  describe "Aggregation Event" $ do
    it "Insert Aggregation Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ insertAggEvent dummyUser dummyAggregation
      (eventInfoEvent evInfo) `shouldBe` dummyAggEvent

    it "Should not allow duplicate Aggregation events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertAggEvent dummyUser dummyAggregation
      res <- runAppM @_ @AppError scsContext $ insertAggEvent dummyUser dummyAggregation
      res `shouldSatisfy` isLeft

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        (evInfo, _) <- insertAggEvent dummyUser dummyAggregation
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (eventInfoEvent evInfo, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyAggEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ insertTransfEvent dummyUser dummyTransformation
      (eventInfoEvent evInfo) `shouldBe` dummyTransfEvent

    it "Should not allow duplicate Transformation events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertTransfEvent dummyUser dummyTransformation
      res <- runAppM @_ @AppError scsContext $ insertTransfEvent dummyUser dummyTransformation
      res `shouldSatisfy` isLeft

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        (evInfo, _) <- insertTransfEvent dummyUser dummyTransformation
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (eventInfoEvent evInfo, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyTransfEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transaction Event" $ do
  -- The otherUserIds are being stubbed out here
    it "Insert Transaction Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ insertTransactEvent dummyUser (dummyTransaction $ ST.UserId dummyId :| [])
      (eventInfoEvent evInfo) `shouldBe` dummyTransactEvent

    it "Should not allow duplicate Transaction events" $ \scsContext -> do
      _res <- runAppM @_ @AppError scsContext $ insertTransactEvent dummyUser (dummyTransaction $ ST.UserId dummyId :| [])
      res <- runAppM @_ @AppError scsContext $ insertTransactEvent dummyUser (dummyTransaction $ ST.UserId dummyId :| [])
      res `shouldSatisfy` isLeft

    it "Should not allow duplicate users" $ \scsContext -> do
      res <- runAppM @_ @AppError scsContext $ insertTransactEvent dummyUser (dummyTransaction $ ST.UserId dummyId :| [ST.UserId dummyId])
      res `shouldSatisfy` isLeft

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        (evInfo, _) <- insertTransactEvent dummyUser (dummyTransaction $ ST.UserId dummyId :| [])
        evtList <- listEvents dummyUser (LabelEPCUrn dummyLabelEpcUrn)
        pure (eventInfoEvent evInfo, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyTransactEvent
          evtList `shouldBe` [insertedEvent]

  describe "getUser tests" $ do
    it "getUser test by Id" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
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
-- Specifically, add a few users, and see if the search works properly;
-- search for just gs1companyprefix;
-- search for just last name; search for both;
-- search with no params given

    it "getUser test by Company Id" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        users <- userSearch dummyUser (Just $ newUserCompany dummyNewUser) Nothing
        -- ^ The argument dummyUser is ignored
        pure (uid, users)
      case res of
        (_, []) -> fail "Received Nothing for user"
        (uid, [user]) ->
          user `shouldSatisfy`
            (\u ->
              (userId u == uid) &&
              (userFirstName u == newUserFirstName dummyNewUser) &&
              (userLastName u == newUserLastName dummyNewUser)
            )
        (_, _) -> fail "There should only be one result in this test"


    it "getUser test by last name" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uidSmith <- addUser dummyNewUser
        _uid_2 <- addUser $ ST.NewUser "000" (unsafeMkEmailAddress "jordan@gmail.com") "Bob" "Jordan" (GS1CompanyPrefix "fake_1 Ltd") "password"
        _uid_3 <- addUser $ ST.NewUser "000" (unsafeMkEmailAddress "james@gmail.com") "Bob" "James" (GS1CompanyPrefix "fake_2 Ltd") "password"

        userSmith <- userSearch dummyUser Nothing (Just "Smith")
        -- ^ The argument dummyUser is ignored
        pure (uidSmith, userSmith)
      case res of
        (_, []) -> fail "Received Nothing for Smith"
        (uidSmith, [smith]) ->
          smith `shouldSatisfy`
            (\u ->
              (userId u == uidSmith) &&
              (userFirstName u == newUserFirstName dummyNewUser) &&
              (userLastName u == newUserLastName dummyNewUser)
            )
        (_, _) -> fail "There should only be one result in this test"

    it "getUser test by last name and company id" $ \scsContext -> do
      res <- testAppM scsContext $ do
        _uid_1 <- addUser dummyNewUser
        uidRealSmith <- addUser $ ST.NewUser "000" (unsafeMkEmailAddress "jordan@gmail.com") "Bob" "Smith" (GS1CompanyPrefix "Real Smith Ltd") "password"
        _uid_3 <- addUser $ ST.NewUser "000" (unsafeMkEmailAddress "james@gmail.com") "Bob" "James" (GS1CompanyPrefix "Fake Jordan Ltd") "password"

        userSmith <- userSearch dummyUser (Just $ GS1CompanyPrefix "Real Smith Ltd") (Just "Smith")
        -- ^ The argument dummyUser is ignored
        pure (uidRealSmith, userSmith)
      case res of
        (_, []) -> fail "Received Nothing for Smith"
        (uidSmith, [smith]) ->
          smith `shouldSatisfy`
            (\u ->
              (userId u == uidSmith) &&
              (userFirstName u == "Bob") &&
              (userLastName u == "Smith")
            )
        (_, _) -> fail "There should only be one result in this test"

    it "getUser test with no param" $ \scsContext -> do
      res <- testAppM scsContext $ do
        _uid <- addUser dummyNewUser
        users <- userSearch dummyUser Nothing Nothing
        -- ^ The argument dummyUser is ignored
        pure users
      res `shouldBe` []

  describe "Contacts" $ do
    (after_ clearContact) . describe "Contacts" $
      describe "Add contact" $
        it "addContact simple" $ \scsContext -> do
          let myContact = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
          (hasBeenAdded, isContact) <- testAppM scsContext $ do
            uid <- addUser dummyNewUser
            user <- runDb $ getUser . newUserEmailAddress $ dummyNewUser
            myContactUid <- addUser myContact
            hasBeenAdded <- addContact (fromJust user) myContactUid
            isContact <- runDb $ isExistingContact uid myContactUid
            pure (hasBeenAdded, isContact)
          hasBeenAdded `shouldBe` True
          isContact `shouldBe` True

    describe "Remove contact" $ do
      it "Simple remove one" $ \scsContext -> do
        -- Adding the contact first
        (hasBeenAdded, hasBeenRemoved) <- testAppM scsContext $ do
          void $ addUser dummyNewUser
          mUser <- runDb $ getUser $ newUserEmailAddress dummyNewUser
          let myContact = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- addUser myContact
          hasBeenAdded <- addContact user myContactUid
        -- removing the contact now
          hasBeenRemoved <- removeContact user myContactUid
          pure (hasBeenAdded, hasBeenRemoved)
        hasBeenAdded `shouldBe` True
        hasBeenRemoved `shouldBe` True
      it "Remove wrong contact" $ \scsContext -> do
        -- Adding the contact first
        (hasBeenAdded, hasBeenRemoved) <- testAppM scsContext $ do
          void $ addUser dummyNewUser
          mUser <- runDb $ getUser $ newUserEmailAddress dummyNewUser
        -- Add a new user who is NOT a contact
          otherUserId <- addUser $ makeDummyNewUser (unsafeMkEmailAddress "other@gmail.com")
          let myContact = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- addUser myContact
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
          void $ addUser dummyNewUser
          mUser <- runDb $ getUser $ newUserEmailAddress dummyNewUser
          let myContact = makeDummyNewUser (unsafeMkEmailAddress "first@gmail.com")
              user = fromJust mUser
          myContactUid <- addUser myContact
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
          void $ addUser dummyNewUser
          myContactUid_1 <- addUser myContact_1
          myContactUid_2 <- addUser myContact_2
          myContactUid_3 <- addUser myContact_3
          myContactUid_4 <- addUser myContact_4

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
  let connectionString = getDatabaseConnectionString testDbConnectionStringSCS
  conn <- connectPostgreSQL connectionString
  void $ execute_ conn "DELETE FROM contacts;"
