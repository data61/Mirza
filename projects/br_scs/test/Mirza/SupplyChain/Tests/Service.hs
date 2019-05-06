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

import           Mirza.SupplyChain.Tests.Dummies

-- import           Mirza.SupplyChain.Auth
import           Mirza.SupplyChain.Database.Schema            as Schema
import           Mirza.SupplyChain.EventUtils
import           Mirza.SupplyChain.Handlers.EventRegistration
import           Mirza.SupplyChain.Handlers.Queries
import           Mirza.SupplyChain.Types                      as ST

import           Control.Monad.Except                         (catchError)

import           Data.Either                                  (isLeft)
import           Data.List.NonEmpty                           (NonEmpty (..))

import           Data.Text.Encoding                           (encodeUtf8)
import           Text.Email.Validate                          (toByteString)

import           Servant

import           Database.Beam
import           GHC.Stack                                    (HasCallStack)

import           Test.Hspec

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
    it "addUser test users with duplicate emails" $ \scsContext ->
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
      (evInfo, _) <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        insertObjectEvent dummyUser{userId = uid} dummyObject
      (eventInfoEvent evInfo) `shouldBe` dummyObjEvent

    it "Should not allow duplicate Object events" $ \scsContext -> do
      (Right uid) <- runAppM @_ @AppError scsContext $ addUser dummyNewUser
      -- uid `shouldSatisfy` isRight
      _res <- runAppM @_ @AppError scsContext $ insertObjectEvent dummyUser{userId = uid} dummyObject
      res <- runAppM @_ @AppError scsContext $ insertObjectEvent dummyUser{userId = uid} dummyObject
      res `shouldSatisfy` isLeft

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        (evInfo, _) <-  insertObjectEvent dummyUser{userId = uid} dummyObject
        evtList <- listEvents dummyUser{userId = uid} (LabelEPCUrn dummyLabelEpcUrn)
        pure (eventInfoEvent evInfo, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyObjEvent
          evtList `shouldBe` [insertedEvent]

  describe "Aggregation Event" $ do
    it "Insert Aggregation Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        insertAggEvent dummyUser{userId = uid} dummyAggregation
      (eventInfoEvent evInfo) `shouldBe` dummyAggEvent

    it "Should not allow duplicate Aggregation events" $ \scsContext -> do
      (Right uid) <- runAppM @_ @AppError scsContext $ addUser dummyNewUser
      _res <- runAppM @_ @AppError scsContext $ insertAggEvent dummyUser{userId = uid} dummyAggregation
      res <- runAppM @_ @AppError scsContext $ insertAggEvent dummyUser{userId = uid} dummyAggregation
      res `shouldSatisfy` isLeft

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        (evInfo, _) <- insertAggEvent dummyUser{userId = uid} dummyAggregation
        evtList <- listEvents dummyUser{userId = uid} (LabelEPCUrn dummyLabelEpcUrn)
        pure (eventInfoEvent evInfo, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyAggEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transformation Event" $ do
    it "Insert Transformation Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        insertTransfEvent dummyUser{userId = uid} dummyTransformation
      (eventInfoEvent evInfo) `shouldBe` dummyTransfEvent

    it "Should not allow duplicate Transformation events" $ \scsContext -> do
      (Right uid) <- runAppM @_ @AppError scsContext $ addUser dummyNewUser
      _res <- runAppM @_ @AppError scsContext $ insertTransfEvent dummyUser{userId = uid} dummyTransformation
      res <- runAppM @_ @AppError scsContext $ insertTransfEvent dummyUser{userId = uid} dummyTransformation
      res `shouldSatisfy` isLeft

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        (evInfo, _) <- insertTransfEvent dummyUser{userId = uid} dummyTransformation
        evtList <- listEvents dummyUser{userId = uid} (LabelEPCUrn dummyLabelEpcUrn)
        pure (eventInfoEvent evInfo, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyTransfEvent
          evtList `shouldBe` [insertedEvent]

  describe "Transaction Event" $ do
  -- The otherUserIds are being stubbed out here
    it "Insert Transaction Event" $ \scsContext -> do
      (evInfo, _) <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        insertTransactEvent dummyUser{userId = uid} (dummyTransaction $ uid :| [])
      (eventInfoEvent evInfo) `shouldBe` dummyTransactEvent

    it "Should not allow duplicate Transaction events" $ \scsContext -> do
      (Right uid) <- runAppM @_ @AppError scsContext $ addUser dummyNewUser
      _res <- runAppM @_ @AppError scsContext $ insertTransactEvent dummyUser{userId = uid} (dummyTransaction $ ST.UserId dummyId :| [])
      res <- runAppM @_ @AppError scsContext $ insertTransactEvent dummyUser{userId = uid} (dummyTransaction $ ST.UserId dummyId :| [])
      res `shouldSatisfy` isLeft

    it "Should not allow duplicate users" $ \scsContext -> do
      (Right uid) <- runAppM @_ @AppError scsContext $ addUser dummyNewUser
      res <- runAppM @_ @AppError scsContext $ insertTransactEvent dummyUser{userId = uid} (dummyTransaction $ ST.UserId dummyId :| [ST.UserId dummyId])
      res `shouldSatisfy` isLeft

    it "List event" $ \scsContext -> do
      res <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        (evInfo, _) <- insertTransactEvent dummyUser{userId = uid} (dummyTransaction $ uid :| [])
        evtList <- listEvents dummyUser{userId = uid} (LabelEPCUrn dummyLabelEpcUrn)
        pure (eventInfoEvent evInfo, evtList)
      case res of
        (insertedEvent, evtList) -> do
          insertedEvent `shouldBe` dummyTransactEvent
          evtList `shouldBe` [insertedEvent]

  describe "getUser tests" $
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

  describe "DWhere" $
    it "Insert and find DWhere" $ \scsContext -> do
      insertedDWhere <- testAppM scsContext $ do
        uid <- addUser dummyNewUser
        (_, eventId) <- insertObjectEvent dummyUser{userId = uid} dummyObject
        runDb $ findDWhere eventId
      insertedDWhere `shouldBe` Just dummyDWhere
