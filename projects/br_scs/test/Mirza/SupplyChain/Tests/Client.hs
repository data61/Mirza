{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Tests.Client where

import           Control.Exception                     (bracket)

import           Servant.API.BasicAuth

import           Data.Either                           (fromRight, isLeft,
                                                        isRight)
import           Data.UUID                             (nil)

import           Data.List.NonEmpty                    (NonEmpty (..))

import           Data.Text.Encoding                    (encodeUtf8)

import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit

import qualified Mirza.BusinessRegistry.Types          as BT
import           Mirza.SupplyChain.Types               as ST

import qualified Mirza.BusinessRegistry.Client.Servant as BRClient
import           Mirza.SupplyChain.Client.Servant

import           Mirza.Common.Utils                    (readJWK, mockURI)

import           Mirza.Common.Tests.InitClient         (TestData (..), endApps,
                                                        runApps)
import           Mirza.SupplyChain.Database.Schema     as Schema

import           Mirza.BusinessRegistry.Client.Servant (addPublicKey)
import           Mirza.BusinessRegistry.Tests.Utils    (goodRsaPrivateKey,
                                                        goodRsaPublicKey)
import           Mirza.BusinessRegistry.Client.Servant (authDataToTokenTodoRemove)

import           Mirza.Common.Tests.ServantUtils
import           Mirza.Common.Tests.Utils
import           Mirza.SupplyChain.Tests.Dummies

import           Data.GS1.EventId                      as EvId

import           Control.Monad.Except
import           Control.Monad.Identity
import           Crypto.JOSE                           (Alg (RS256),
                                                        newJWSHeader, signJWS)
import qualified Crypto.JOSE                           as JOSE
import           Crypto.JOSE.Types                     (Base64Octets (..))

import           Data.GS1.EPC                          (GS1CompanyPrefix (..))
import           Text.Email.Validate                   (toByteString)

-- === SCS Client tests


userABC :: NewUser
userABC = NewUser
  { newUserPhoneNumber = "0400 111 222"
  , newUserEmailAddress = unsafeMkEmailAddress "abc@example.com"
  , newUserFirstName = "User ABC"
  , newUserLastName = "Giver"
  , newUserCompany = GS1CompanyPrefix "something"
  , newUserPassword = "re4lly$ecret14!" }

authABC :: BasicAuthData
authABC = BasicAuthData
  (toByteString . newUserEmailAddress $ userABC)
  (encodeUtf8   . newUserPassword     $ userABC)


userDEF :: NewUser
userDEF = NewUser
  { newUserPhoneNumber = "0400 111 222"
  , newUserEmailAddress = unsafeMkEmailAddress "def@example.com"
  , newUserFirstName = "User DEF"
  , newUserLastName = "Receiver"
  , newUserCompany = GS1CompanyPrefix "something"
  , newUserPassword = "re4lly$ecret14!"}

authDEF :: BasicAuthData
authDEF = BasicAuthData
  (toByteString . newUserEmailAddress $ userDEF)
  (encodeUtf8   . newUserPassword     $ userDEF)

clientSpec :: IO TestTree
clientSpec = do

  let userCreationTests = testCaseSteps "Adding new users" $ \step ->
        -- bracket runSCSApp endWaiApp $ \(_tid,baseurl) -> do
        bracket runApps endApps $ \testData -> do
          let baseurl = scsBaseUrl testData
              http = runClient baseurl

          let user1 = userABC
              user2 = userABC {newUserEmailAddress= unsafeMkEmailAddress "different@example.com"}
              -- Same email address as user1 other fields different.
              userSameEmail = userABC {newUserFirstName="First"}

          step "Can create a new user"
          http (addUser user1)
            `shouldSatisfyIO` isRight

          step "Can't create a new user with the same email address"
          http (addUser userSameEmail)
            `shouldSatisfyIO` isLeft

          step "Can create a second user"
          http (addUser user2)
            `shouldSatisfyIO` isRight

          step "Should be able to authenticate"
          http (contactsInfo authABC)
            `shouldSatisfyIO` isRight

          step "Should fail to authenticate with unknown user"
          http (contactsInfo (BasicAuthData "xyz@example.com" "notagoodpassword"))
            `shouldSatisfyIO` isLeft

  let eventInsertionTests = testCaseSteps "User can add single events" $ \step ->
        bracket runApps endApps $ \testData -> do

          let scsUrl = scsBaseUrl testData
              http = runClient scsUrl

          -- Add a user so that we can authenticate for these tests.
          http (addUser userABC) `shouldSatisfyIO` isRight

          step "User Can insert Object events"
            -- TODO: Events need their EventId returned to user
          http (insertObjectEvent authABC dummyObject)
            `shouldSatisfyIO` isRight

          step "User Can insert Aggregation events"
          http (insertAggEvent authABC dummyAggregation)
            `shouldSatisfyIO` isRight

          step "Adding further users for transaction events"
          resReceiver <- http (addUser userDEF)
          resReceiver `shouldSatisfy` isRight
          let (Right userIdSigning) = resReceiver

          step "User Can insert Transaction events"
          http (insertTransactEvent authABC $ dummyTransaction $ userIdSigning :| [])
            `shouldSatisfyIO` isRight

          step "User Can insert Transformation events"
          http (insertTransfEvent authABC dummyTransformation)
            `shouldSatisfyIO` isRight

  let _eventSignTests = testCaseSteps "eventSign" $ \step ->
        bracket runApps endApps $ \testData -> do

          let scsUrl = scsBaseUrl testData
              brUrl = brBaseUrl testData
              httpSCS = runClient scsUrl
              httpBR = runClient brUrl
              brAuthUser = brAuthData testData

          step "Adding a new user to SCS"
          httpSCS (addUser userABC) `shouldSatisfyIO` isRight

          step "Adding the same user to BR"
          let prefix = GS1CompanyPrefix "1000001"
          let userBR = BT.NewUser
                          "EventSign Test Same User OAuthSub"
                          (unsafeMkEmailAddress "abc@example.com")
                          prefix
                          "Biz Johnny"
                          "Smith Biz"
                          "0400 111 222"


          let businessName = "Business Name"
              business = BT.NewBusiness prefix businessName (mockURI businessName)
          httpBR (BRClient.addBusiness brAuthUser business)
            `shouldSatisfyIO` isRight

          httpBR (BRClient.addUser brAuthUser userBR) `shouldSatisfyIO` isRight

          step "Tying the user with a good key"
          Just goodPubKey <- goodRsaPublicKey
          Just goodPrivKey <- goodRsaPrivateKey
          keyIdResponse <- httpBR (addPublicKey (authDataToTokenTodoRemove authABC) goodPubKey Nothing)
          keyIdResponse `shouldSatisfy` isRight
          let keyId = fromRight (BRKeyId nil) keyIdResponse

          step "Inserting the object event"
          objInsertionResponse <- httpSCS (insertObjectEvent authABC dummyObject)
          objInsertionResponse `shouldSatisfy` isRight
          let (EventInfo _ _ _ (Base64Octets to_sign_event) _, (Schema.EventId eventId)) = fromRight (error "Should be right") objInsertionResponse

          step "Signing the key"
          Right mySig <- runExceptT @JOSE.Error (
                    signJWS to_sign_event (Identity (newJWSHeader ((), RS256),goodPrivKey))
                    )
          let mySignedEvent = SignedEvent (EvId.EventId eventId) keyId mySig

          httpSCS (eventSign authABC mySignedEvent) `shouldSatisfyIO` isRight

  let _transactionEventTest = testCaseSteps
        "Signing and counter-signing a transaction event" $ \step ->
        bracket runApps endApps $ \testData -> do

          let scsUrl = scsBaseUrl testData
              brUrl = brBaseUrl testData
              httpSCS = runClient scsUrl
              httpBR = runClient brUrl
              globalAuthData = brAuthData testData

          -- ===============================================
          -- Giving user
          -- ===============================================

          step "Adding a giver user to SCS"
          uidGiver <- httpSCS (addUser userABC)
          uidGiver `shouldSatisfy` isRight
          let (Right userIdGiver) = uidGiver

          step "Adding business for the Giver"
          let prefixGiver = GS1CompanyPrefix "1000001"
              businessGiverName = "Giver Biz"
              businessGiver = BT.NewBusiness prefixGiver businessGiverName (mockURI businessGiverName)
          httpBR (BRClient.addBusiness globalAuthData businessGiver)
            `shouldSatisfyIO` isRight

          step "Adding the giver user to BR"
          let userBRGiver = BT.NewUser
                          "EventSign Test Giver OAuthSub"
                          (unsafeMkEmailAddress "abc@example.com")
                          prefixGiver
                          "Biz Giver"
                          "Smith Biz"
                          "0400 111 222"
          httpBR (BRClient.addUser globalAuthData userBRGiver) `shouldSatisfyIO` isRight

          step "Tying the giver user with a good key"
          Just goodPubKeyGiver <- readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/4096bit_rsa_pub.json"
          Just goodPrivKeyGiver <- readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/4096bit_rsa.json"
          keyIdResponseGiver <- httpBR (addPublicKey (authDataToTokenTodoRemove authABC) goodPubKeyGiver Nothing)
          keyIdResponseGiver `shouldSatisfy` isRight
          let keyIdGiver = fromRight (BRKeyId nil) keyIdResponseGiver

          -- ===============================================
          -- Receiving user
          -- ===============================================
          step "Adding receiving user to SCS"
          uidReceiver <- httpSCS (addUser userDEF)
          uidReceiver `shouldSatisfy` isRight
          let (Right userIdReceiver) = uidReceiver

          step "Adding business for receiver"
          let prefixReceiver = GS1CompanyPrefix "1000002"
              businessReceiverName = "Receiving Biz"
              businessReceiver = BT.NewBusiness prefixReceiver businessReceiverName (mockURI businessReceiverName)

          httpBR (BRClient.addBusiness globalAuthData businessReceiver)
            `shouldSatisfyIO` isRight

          step "Adding the receiving user to BR"
          let userBRReceiver = BT.NewUser
                          "EventSign Test Reciever OAuthSub"
                          (unsafeMkEmailAddress "def@example.com")
                          prefixReceiver
                          "Biz Receiver"
                          "Smith Biz"
                          "0400 123 432"
          httpBR (BRClient.addUser globalAuthData userBRReceiver) `shouldSatisfyIO` isRight


          -- step "Signing the event with the second user"
          step "Tying the receiver user with a good key"
          Just goodPubKeyReceiver <- readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/16384bit_rsa_pub.json"
          Just goodPrivKeyReceiver <- readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/16384bit_rsa.json"
          keyIdResponseReceiver <- httpBR (addPublicKey (authDataToTokenTodoRemove authDEF) goodPubKeyReceiver Nothing)
          keyIdResponseReceiver `shouldSatisfy` isRight
          let keyIdReceiver = fromRight (BRKeyId nil) keyIdResponseReceiver

          step "Inserting the transaction event with the giver user"
          let myTransactionEvent = dummyTransaction $ userIdReceiver :| []
          transactInsertionResponse <- httpSCS (insertTransactEvent authABC myTransactionEvent)
          transactInsertionResponse `shouldSatisfy` isRight
          let (_transactEvInfo@(EventInfo insertedTransactEvent _ _ (Base64Octets to_sign_transact_event) _), (Schema.EventId transactEvId)) =
                  fromRight (error "Should be right") transactInsertionResponse
              transactEventId = EvId.EventId transactEvId

          step "Retrieving the event info"

          eventInfoResult <- httpSCS (eventInfo authABC transactEventId)
          eventInfoResult `shouldSatisfy` isRight
          let (Right eInfo) = eventInfoResult

          step "Checking that we got the correct event back"
          let retrievedTransactEvent = (eventInfoEvent eInfo)
          retrievedTransactEvent `shouldBe` insertedTransactEvent

          step "Checking event blockchain status"
          let eventStatus = (eventInfoBlockChainStatus eInfo)
          eventStatus `shouldBe` NeedMoreSignatures

          step "Checking that receiving user is among the unsigned users"
          let unsignedUsers = (eventInfoUnsignedUsers eInfo)
          unsignedUsers `shouldBe` [userIdGiver, userIdReceiver]

          step "Signing the transaction event with Giver"
          Right giverSigTransact <- runExceptT @JOSE.Error $
                    signJWS to_sign_transact_event (Identity (newJWSHeader ((), RS256), goodPrivKeyGiver))
          let myTransactSignedEvent = SignedEvent transactEventId keyIdGiver giverSigTransact
          httpSCS (eventSign authABC myTransactSignedEvent) `shouldSatisfyIO` isRight

          step "Signing the transaction event with the receiver user"
          Right receiverSig <- runExceptT @JOSE.Error $
                    signJWS to_sign_transact_event (Identity (newJWSHeader ((), RS256),goodPrivKeyReceiver))
          let receiverSignedEvent = SignedEvent transactEventId keyIdReceiver receiverSig

          httpSCS (eventSign authDEF receiverSignedEvent) `shouldSatisfyIO` isRight

          step "Retrieving the event info again"
          eventInfoResult2 <- httpSCS (eventInfo authABC transactEventId)
          eventInfoResult2 `shouldSatisfy` isRight

          step "Checking that the status of the event has changed to Ready"
          let (Right eInfo2) = eventInfoResult2
          let eventStatus2 = (eventInfoBlockChainStatus eInfo2)
          eventStatus2 `shouldBe` ReadyAndWaiting


  let healthTests = testCaseSteps "Provides health status" $ \step ->
        bracket runApps endApps $ \testData-> do
          let baseurl = scsBaseUrl testData
              http = runClient baseurl

          step "Status results in 200"
          healthResult <- http (health)
          healthResult `shouldSatisfy` isRight
          healthResult `shouldBe` (Right HealthResponse)



  pure $ testGroup "Supply Chain Service Client Tests"
        [ userCreationTests
        , eventInsertionTests
        -- TODO: Reinclude the following test cases which fail because we have not sorted out auth for test cases yet.
        --, eventSignTests
        --, transactionEventTest
        , healthTests
        ]


