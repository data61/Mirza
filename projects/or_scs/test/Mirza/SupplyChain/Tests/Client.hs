{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Tests.Client where

import           Control.Exception                     (bracket)

import           Data.Either                           (fromRight, isRight)
import           Data.UUID                             (nil)

import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit

import qualified Mirza.OrgRegistry.Types          as ORT
import           Mirza.SupplyChain.Types               as ST

import qualified Mirza.OrgRegistry.Client.Servant as ORClient
import           Mirza.SupplyChain.Client.Servant

import           Mirza.Common.Utils                    (mockURI, readJWK)

import           Mirza.Common.Tests.InitClient         (TestData (..), endApps,
                                                        runApps)
import           Mirza.SupplyChain.Database.Schema     as Schema

import           Mirza.OrgRegistry.Client.Servant (addPublicKey)
import           Mirza.OrgRegistry.Tests.Utils    (goodRsaPrivateKey,
                                                        goodRsaPublicKey)

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

-- === SCS Client tests

clientSpec :: IO TestTree
clientSpec = do

  let eventInsertionTests = testCaseSteps "Can add single events" $ \step ->
        bracket runApps endApps $ \testData -> do

          let scsUrl = scsBaseUrl testData
              http = runClient scsUrl

          step "Can insert Object events"
            -- TODO: Events need their EventId returned to user
          http (insertGS1Event dummyObjEvent)
            `shouldSatisfyIO` isRight

          step "Can insert Aggregation events"
          http (insertGS1Event dummyAggEvent)
            `shouldSatisfyIO` isRight

          step "Can insert Transaction events"
          http (insertGS1Event dummyTransactEvent)
            `shouldSatisfyIO` isRight

          step "Can insert Transformation events"
          http (insertGS1Event dummyTransfEvent)
            `shouldSatisfyIO` isRight

  let _eventSignTests = testCaseSteps "eventSign" $ \step ->
        bracket runApps endApps $ \testData -> do

          let scsUrl = scsBaseUrl testData
              orUrl = orBaseUrl testData
              httpSCS = runClient scsUrl
              httpOR = runClient orUrl
              orAuthUser = orAuthData testData

          step "Adding a user to OR"
          let prefix = GS1CompanyPrefix "1000001"
          -- TODO: Create a user for associating with tests.
          --let userOR = ORT.NewUser "EventSign Test Same User OAuthSub"

          -- TODO Note: The org will now be associated with the orAuthUser
          -- and not created user, expect this may have to be fixed when we
          -- devise a means of authing test users.
          let orgName = "Org Name"
              org = ORT.PartialNewOrg orgName (mockURI orgName)
          httpOR (ORClient.addOrg orAuthUser prefix org)
            `shouldSatisfyIO` isRight

          -- TODO: Create a user for associating with tests.
          --httpOR (ORClient.addUser orAuthUser userOR) `shouldSatisfyIO` isRight

          step "Tying the user with a good key"
          Just goodPubKey <- goodRsaPublicKey
          Just goodPrivKey <- goodRsaPrivateKey
          keyIdResponse <- httpOR (addPublicKey orAuthUser goodPubKey Nothing)
          keyIdResponse `shouldSatisfy` isRight
          let keyId = fromRight (ORKeyId nil) keyIdResponse

          step "Inserting the object event"
          objInsertionResponse <- httpSCS (insertGS1Event dummyObjEvent)
          objInsertionResponse `shouldSatisfy` isRight
          let (EventInfo _ (Base64Octets to_sign_event) _, (Schema.EventId eventId)) = fromRight (error "Should be right") objInsertionResponse

          step "Signing the key"
          Right mySig <- runExceptT @JOSE.Error (
                    signJWS to_sign_event (Identity (newJWSHeader ((), RS256),goodPrivKey))
                    )
          let mySignedEvent = SignedEvent (EvId.EventId eventId) keyId mySig

          httpSCS (eventSign mySignedEvent) `shouldSatisfyIO` isRight

  let _transactionEventTest = testCaseSteps
        "Signing and counter-signing a transaction event" $ \step ->
        bracket runApps endApps $ \testData -> do

          let scsUrl = scsBaseUrl testData
              orUrl = orBaseUrl testData
              httpSCS = runClient scsUrl
              httpOR = runClient orUrl
              globalAuthData = orAuthData testData

          -- ===============================================
          -- Giving user
          -- ===============================================

          -- TODO Note: The org will now be associated with the orAuthUser
          -- and not created user, expect this may have to be fixed when we
          -- devise a means of authing test users.
          step "Adding org for the Giver"
          let prefixGiver = GS1CompanyPrefix "1000001"
              orgGiverName = "Giver Org"
              orgGiver = ORT.PartialNewOrg orgGiverName (mockURI orgGiverName)
          httpOR (ORClient.addOrg globalAuthData prefixGiver orgGiver)
            `shouldSatisfyIO` isRight

          -- TODO: Create a user for associating with tests.
          --step "Adding the giver user to OR"
          --let userORGiver = ORT.NewUser "EventSign Test Giver OAuthSub"
          --httpOR (ORClient.addUser globalAuthData userORGiver) `shouldSatisfyIO` isRight

          step "Tying the giver user with a good key"
          Just goodPubKeyGiver <- readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/4096bit_rsa_pub.json"
          Just goodPrivKeyGiver <- readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/4096bit_rsa.json"
          keyIdResponseGiver <- httpOR (addPublicKey globalAuthData goodPubKeyGiver Nothing)
          keyIdResponseGiver `shouldSatisfy` isRight
          let keyIdGiver = fromRight (ORKeyId nil) keyIdResponseGiver


          -- TODO Note: The org will now be associated with the orAuthUser
          -- and not created user, expect this will have to be fixed when we
          -- devise a means of authing test users.
          step "Adding org for receiver"
          let prefixReceiver = GS1CompanyPrefix "1000002"
              orgReceiverName = "Receiving Org"
              orgReceiver = ORT.PartialNewOrg orgReceiverName (mockURI orgReceiverName)

          httpOR (ORClient.addOrg globalAuthData prefixReceiver orgReceiver)
            `shouldSatisfyIO` isRight

          -- TODO: Create a user for associating with tests.
          --step "Adding the receiving user to OR"
          --let userORReceiver = ORT.NewUser "EventSign Test Reciever OAuthSub"
          --httpOR (ORClient.addUser globalAuthData userORReceiver) `shouldSatisfyIO` isRight


          -- step "Signing the event with the second user"
          step "Tying the receiver user with a good key"
          Just goodPubKeyReceiver <- readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/16384bit_rsa_pub.json"
          Just goodPrivKeyReceiver <- readJWK "./test/Mirza/Common/TestData/testKeys/goodJWKs/16384bit_rsa.json"
          keyIdResponseReceiver <- httpOR (addPublicKey globalAuthData goodPubKeyReceiver Nothing)
          keyIdResponseReceiver `shouldSatisfy` isRight
          let keyIdReceiver = fromRight (ORKeyId nil) keyIdResponseReceiver

          step "Inserting the transaction event with the giver user"
          transactInsertionResponse <- httpSCS (insertGS1Event dummyTransactEvent)
          transactInsertionResponse `shouldSatisfy` isRight
          let (_transactEvInfo@(EventInfo insertedTransactEvent (Base64Octets to_sign_transact_event) _), (Schema.EventId transactEvId)) =
                  fromRight (error "Should be right") transactInsertionResponse
              transactEventId = EvId.EventId transactEvId

          step "Retrieving the event info"

          eventInfoResult <- httpSCS (eventInfo transactEventId)
          eventInfoResult `shouldSatisfy` isRight
          let (Right eInfo) = eventInfoResult

          step "Checking that we got the correct event back"
          let retrievedTransactEvent = (eventInfoEvent eInfo)
          retrievedTransactEvent `shouldBe` insertedTransactEvent

          step "Checking event blockchain status"
          let eventStatus = (eventInfoBlockChainStatus eInfo)
          eventStatus `shouldBe` NeedMoreSignatures

          step "Signing the transaction event with Giver"
          Right giverSigTransact <- runExceptT @JOSE.Error $
                    signJWS to_sign_transact_event (Identity (newJWSHeader ((), RS256), goodPrivKeyGiver))
          let myTransactSignedEvent = SignedEvent transactEventId keyIdGiver giverSigTransact
          httpSCS (eventSign myTransactSignedEvent) `shouldSatisfyIO` isRight

          step "Signing the transaction event with the receiver user"
          Right receiverSig <- runExceptT @JOSE.Error $
                    signJWS to_sign_transact_event (Identity (newJWSHeader ((), RS256),goodPrivKeyReceiver))
          let receiverSignedEvent = SignedEvent transactEventId keyIdReceiver receiverSig

          httpSCS (eventSign receiverSignedEvent) `shouldSatisfyIO` isRight

          step "Retrieving the event info again"
          eventInfoResult2 <- httpSCS (eventInfo transactEventId)
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
          healthResult <- http health
          healthResult `shouldSatisfy` isRight
          healthResult `shouldBe` (Right HealthResponse)



  pure $ testGroup "Supply Chain Service Client Tests"
        [ eventInsertionTests
        -- TODO: Reinclude the following test cases which fail because we have not sorted out auth for test cases yet.
        --, eventSignTests
        --, transactionEventTest
        , healthTests
        ]


