{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.SupplyChain.Tests.Client where


import           Control.Concurrent                (ThreadId)
import           Control.Exception                 (bracket)

import           Servant.API.BasicAuth
import           Servant.Client                    (BaseUrl)

import           Data.Either                       (isLeft, isRight, fromRight)
import           Data.UUID                         (nil)

import           Data.Text.Encoding                (encodeUtf8)

import           Test.Tasty.Hspec
import           Test.Tasty
import           Test.Tasty.HUnit

import           Katip                             (Severity (DebugS))

import           Mirza.SupplyChain.Main            (ServerOptions (..),
                                                    initApplication,
                                                    initSCSContext)
import           Mirza.SupplyChain.Types           as ST
import           Mirza.SupplyChain.Client.Servant
import           Mirza.BusinessRegistry.Client.Servant (revokePublicKey,
                                                        addPublicKey)
import           Mirza.SupplyChain.Tests.Dummies
import           Mirza.SupplyChain.Tests.Settings
import           Mirza.Common.Test.ServantUtil
import           Mirza.SupplyChain.Database.Schema as Schema

import           Database.Beam.Query               (delete, runDelete, val_)

-- import           Mirza.Common.Time                (ExpirationTime (..))
-- import           Data.Time.Clock                  (addUTCTime, getCurrentTime)

import           Data.GS1.EPC                     (GS1CompanyPrefix (..))
import           Data.GS1.EventId                 as EvId

import           Mirza.BusinessRegistry.Tests.Utils

-- === SCS Client tests

userABC :: NewUser
userABC = NewUser
  { newUserPhoneNumber = "0400 111 222"
  , newUserEmailAddress = EmailAddress "abc@example.com"
  , newUserFirstName = "Biz Johnny"
  , newUserLastName = "Smith Biz"
  , newUserCompany = GS1CompanyPrefix "something"
  , newUserPassword = "re4lly$ecret14!"}

authABC :: BasicAuthData
authABC = BasicAuthData
  (encodeUtf8 . getEmailAddress . newUserEmailAddress $ userABC)
  (encodeUtf8 . newUserPassword                       $ userABC)

runApp :: IO (ThreadId, BaseUrl)
runApp = do
  ctx <- initSCSContext so
  startWaiApp =<< initApplication so ctx

so :: ServerOptions
so = ServerOptions Dev False testDbConnStr "127.0.0.1" 8000 14 8 1 DebugS

clientSpec :: IO TestTree
clientSpec = do
  ctx <- initSCSContext so
  let SupplyChainDb
        usersTable
        businessesTable
        contactsTable
        labelsTable
        whatLabelsTable
        itemsTable
        transformationsTable
        locationsTable
        eventsTable
        whatsTable
        bizTransactionsTable
        whysTable
        wheresTable
        whensTable
        labelEventsTable
        userEventsTable
        signaturesTable
        hashesTable
        blockchainTable
          = supplyChainDb
  flushDbResult <- runAppM @_ @ServiceError ctx $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable $ usersTable
      deleteTable $ businessesTable
      deleteTable $ contactsTable
      deleteTable $ labelsTable
      deleteTable $ whatLabelsTable
      deleteTable $ itemsTable
      deleteTable $ transformationsTable
      deleteTable $ locationsTable
      deleteTable $ eventsTable
      deleteTable $ whatsTable
      deleteTable $ bizTransactionsTable
      deleteTable $ whysTable
      deleteTable $ wheresTable
      deleteTable $ whensTable
      deleteTable $ labelEventsTable
      deleteTable $ userEventsTable
      deleteTable $ signaturesTable
      deleteTable $ hashesTable
      deleteTable $ blockchainTable
  flushDbResult `shouldSatisfy` isRight


  let userCreationTests = testCaseSteps "Adding new users" $ \step ->
        bracket runApp endWaiApp $ \(_tid,baseurl) -> do
          let http = runClient baseurl

          let user1 = userABC
              user2 = userABC {newUserEmailAddress= EmailAddress "different@example.com"}
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
        bracket runApp endWaiApp $ \(_tid,baseurl) -> do
          let http = runClient baseurl
          step "User Can insert Object events"
            -- TODO: Events need their EventId returned to user
          resp <- http (insertObjectEvent authABC dummyObject)
          resp `shouldSatisfy` isRight

          step "User Can insert Aggregation events"
          http (insertAggEvent authABC dummyAggregation)
            `shouldSatisfyIO` isRight

          step "User Can insert Transaction events"
          http (insertTransactEvent authABC dummyTransaction)
            `shouldSatisfyIO` isRight

          step "User Can insert Transformation events"
          http (insertTransfEvent authABC dummyTransformation)
            `shouldSatisfyIO` isRight

  let eventSignTests = testCaseSteps "eventSign" $ \step ->
        bracket runApp endWaiApp $ \(_tid,baseurl) -> do
        let http = runClient baseurl

        -- nowish <- getCurrentTime
        -- let hundredMinutes = 100 * 60
        --     someTimeLater = addUTCTime (hundredMinutes) nowish

        step "Adding a new user"
        uid <- http (addUser userABC)
        uid `shouldSatisfy` isRight

        step "Tying the user with a good key and an expiration time"
        goodKey <- goodRsaPublicKey
        keyIdResponse <- http (addPublicKey authABC goodKey Nothing)
        -- liftIO $ print keyIdResponse
        keyIdResponse `shouldSatisfy` isRight
        let keyId = fromRight (BRKeyId nil) keyIdResponse

        step "Revoking the key"
        http (revokePublicKey authABC keyId) `shouldSatisfyIO` isRight

        step "Inserting the object event"
        objInsertionResponse <- http (insertObjectEvent authABC dummyObject)
        objInsertionResponse `shouldSatisfy` isRight
        let (_insertedEvent, (Schema.EventId eventId)) = fromRight (error "Should be right") objInsertionResponse

          -- Adding the event
        let mySign = ST.Signature "c2FqaWRhbm93ZXIyMw=="
            myDigest = SHA256
            mySignedEvent = SignedEvent (EvId.EventId eventId) keyId mySign myDigest
        -- if this test can proceed after the following statement
        http (eventSign authABC mySignedEvent) `shouldSatisfyIO` isRight
        -- it means the basic functionality of ``eventSign`` function is perhaps done

  pure $ testGroup "Supply Chain Service Client Tests"
        [ userCreationTests
        , eventInsertionTests
        , eventSignTests
        ]

{-
Check Provenance of a labelEPC
where I've used head, you need to use map to actually do it for all elements in the list. I've just done one element for illustrative purposes.
eventList ← listEvents <labelEPC>
let event = head eventList
eventInfo ← eventInfo(eventID)
(sig, uid) = head (signatures eventInfo)
publicKey ← getPublicKey uid
assert $ decrypt(sig, publicKey) == (joseText eventInfo)

Insert an ObjectEvent
Create an ObjectEvent
add it via the API
sign it
Insert an AggregationEvent
Create an aggregation event
add it
sign it

Insert an TransformationEvent
Create an transformation event
add it
sign it

Sign AND countersign a TransactionEvent
(eventID, joseTxt) ← insertTransactionEvent transactionEvent
signedEvent = sign(joseTxt, privKey)
sign(signedEvent)
addUserToEvent(user2ID, eventID)
.. then user2 does the same thing with their priv key, and sends it using the "event/sign" api call.

Check for tampering by comparing to Blockchain hash
eventInfo ← eventInfo(eventID)
joseTxt = joseText eventInfo
expectedHash = hash joseText
blockchainID = blockchainID eventInfo
bcHash = getBlockchainHash(blockchainID)
assert (bcHash == expectedHash)
Get all events that relate to a labelEPC
eventList ← listEvents <labelEPC>
subEvents eventList = [e | e ← eventList, if
(eventType e == aggregationEvent || eventType e == transformationEvent)
then (map subEvents $ map listEvents (getSubEPCs e)]
Keys
add, get, getInfo public key
revoke public key
..these will be moved into the registery soon.
Contacts
Add, remove and search for contacts.


-}
