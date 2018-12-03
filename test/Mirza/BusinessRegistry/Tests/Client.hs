{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Mirza.BusinessRegistry.Tests.Client where

import           Control.Concurrent                    (threadDelay)
import           Control.Exception                     (bracket)

import           Mirza.Common.Tests.ServantUtils

import           Servant.API.BasicAuth
import           Servant.Client

import           Data.ByteString.Lazy                  (ByteString)
import           Data.Text.Encoding                    (encodeUtf8)

import           System.Directory                      (listDirectory)
import           System.FilePath                       ((</>))

import           Control.Monad                         (forM_)
import           Data.Either                           (isLeft, isRight)
import           Data.Either.Utils                     (fromRight)
import           Data.Function                         ((&))
import           Data.List                             (isSuffixOf)
import           Data.Maybe                            (fromJust, isJust,
                                                        isNothing)
import           Data.Time.Clock                       (addUTCTime, diffUTCTime,
                                                        getCurrentTime)
import           Data.UUID                             (nil)
import           Text.Email.Validate                   (toByteString)

import qualified Network.HTTP.Types.Status             as NS

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.GS1.EPC                          (GS1CompanyPrefix (..),
                                                        LocationEPC (SGLN),
                                                        LocationReference (LocationReference))

import           Mirza.BusinessRegistry.Client.Servant
import           Mirza.BusinessRegistry.Types

import           Mirza.Common.Time

import           Mirza.BusinessRegistry.Tests.Utils
import           Mirza.Common.Tests.InitClient
import           Mirza.Common.Tests.Utils

-- === BR Servant Client tests
userABC :: NewUser
userABC = NewUser
  { newUserPhoneNumber = "0400 111 222"
  , newUserEmailAddress = unsafeMkEmailAddress "abc@example.com"
  , newUserFirstName = "Johnny"
  , newUserLastName = "Smith"
  , newUserCompany = GS1CompanyPrefix "something"
  , newUserPassword = "re4lly$ecret14!"}

authABC :: BasicAuthData
authABC = BasicAuthData
  (toByteString . newUserEmailAddress $ userABC)
  (encodeUtf8   . newUserPassword     $ userABC)

clientSpec :: IO TestTree
clientSpec = do
  let businessTests = testCaseSteps "Can create businesses" $ \step ->
        bracket runBRApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid,baseurl,brAuthUser) -> do
          let http = runClient baseurl
              biz1Prefix   = GS1CompanyPrefix "2000001"
              biz1         = NewBusiness biz1Prefix "businessTests_biz1Name"
              biz1Response = newBusinessToBusinessResponse biz1
              biz2Prefix   = GS1CompanyPrefix "2000002"
              biz2         =  NewBusiness biz2Prefix "businessTests_biz2Name"
              biz2Response = newBusinessToBusinessResponse biz2
              biz3Prefix   = GS1CompanyPrefix "3000003"
              biz3         =  NewBusiness biz3Prefix "A strange name"
              biz3Response = newBusinessToBusinessResponse biz3
              -- emptyPrefixBiz = NewBusiness (GS1CompanyPrefix "") "EmptyBusiness"
              -- stringPrefix1Biz = NewBusiness (GS1CompanyPrefix "string") "EmptyBusiness"

          step "Can create a new business"
          addBiz1Result <- http (addBusiness brAuthUser biz1)
          addBiz1Result `shouldSatisfy` isRight
          addBiz1Result `shouldBe` (Right biz1Prefix)

          step "That the added business was added and can be listed."
          http (searchBusinesses Nothing Nothing Nothing) >>=
            either (const $ expectationFailure "Error listing businesses")
                   (`shouldContain` [biz1Response])

          step "Can't add business with the same GS1CompanyPrefix"
          duplicatePrefixResult <- http (addBusiness brAuthUser biz1{newBusinessName = "businessTests_anotherName"})
          duplicatePrefixResult `shouldSatisfy` isLeft
          duplicatePrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          duplicatePrefixResult `shouldSatisfy` (checkFailureMessage "GS1 company prefix already exists.")

          step "Can add a second business"
          addBiz2Result <- http (addBusiness brAuthUser biz2)
          addBiz2Result `shouldSatisfy` isRight
          addBiz2Result `shouldBe` (Right biz2Prefix)

          step "List businesses returns all of the businesses"
          http (searchBusinesses Nothing Nothing Nothing) >>=
              either (const $ expectationFailure "Error listing businesses")
                    (`shouldContain` [ biz1Response
                                        , biz2Response])

          step "Can add a third business"
          addBiz3Result <- http (addBusiness brAuthUser biz3)
          addBiz3Result `shouldSatisfy` isRight
          addBiz3Result `shouldBe` (Right biz3Prefix)

          step "Searching by GS1 ID works"
          searchBiz3Result <- http (searchBusinesses (Just biz3Prefix) Nothing Nothing)
          searchBiz3Result `shouldSatisfy` isRight
          searchBiz3Result `shouldBe` (Right [biz3Response])

          step "Searching by business name works"
          searchBiz3NameResult <- http (searchBusinesses Nothing (Just "strange") Nothing)
          searchBiz3NameResult `shouldSatisfy` isRight
          searchBiz3NameResult `shouldBe` (Right [biz3Response])

          searchBiz12NameResult <- http (searchBusinesses Nothing (Just "Tests_") Nothing)
          searchBiz12NameResult `shouldSatisfy` isRight
          searchBiz12NameResult & either (error "You said this was Right!")
                                         (`shouldContain` [ biz1Response
                                                          , biz2Response])

          -- TODO: Include me (github #205):
          -- step "That the GS1CompanyPrefix can't be empty (\"\")."
          -- emptyPrefixResult <- http (addBusiness brAuthUser emptyPrefixBiz)
          -- emptyPrefixResult `shouldSatisfy` isLeft
          -- emptyPrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- emptyPrefixResult `shouldSatisfy` (checkFailureMessage "TODO")

          -- TODO: Include me (github #205):
          -- This should possibly be changed to something that is within the
          -- type specification but is logically incorrect if the type
          -- constraint is improved.
          -- step "That the GS1CompanyPrefix can't be a string."
          -- stringPrefixResult <- http (addBusiness brAuthUser stringPrefix1Biz)
          -- stringPrefixResult `shouldSatisfy` isLeft
          -- stringPrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- stringPrefixResult `shouldSatisfy` (checkFailureMessage "TODO")


  let userTests = testCaseSteps "Can create users" $ \step ->
        bracket runBRApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid,baseurl,brAuthUser) -> do
          password <- randomPassword

          let http = runClient baseurl
              companyPrefix = (GS1CompanyPrefix "3000001")
              business = NewBusiness companyPrefix "userTests_businessName"

          let user1 = NewUser (unsafeMkEmailAddress "userTests_email1@example.com")
                              password
                              companyPrefix
                              "userTests First Name 1"
                              "userTests Last Name 1"
                              "userTests Phone Number 1"
              user2 = NewUser (unsafeMkEmailAddress "userTests_email2@example.com")
                              password
                              companyPrefix
                              "userTests First Name 2"
                              "userTests Last Name 2"
                              "userTests Phone Number 2"
              -- Same email address as user1 other fields different.
              userSameEmail = NewUser (newUserEmailAddress user1)
                                      password
                                      companyPrefix
                                      "userTests First Name Same Email"
                                      "userTests Last Name Same Email"
                                      "userTests Phone Number Same Email"
              userNonRegisteredBiz = NewUser (unsafeMkEmailAddress "userTests_unregisteredBusiness@example.com")
                                             password
                                             (GS1CompanyPrefix "unregistered")
                                             "userTests First Name Unregistered Business"
                                             "userTests Last Name Unregistered Business"
                                             "userTests Phone Number Unregistered Business"
              -- userEmptyEmail = NewUser (EmailAddress "")
              --                          password
              --                          companyPrefix
              --                          "userTests First Name Empty Email"
              --                          "userTests Last Name Empty Email"
              --                          "userTests Phone Number Empty Email"
              -- userEmptyPassword = NewUser (EmailAddress "userTests_emptyPassword@example.com")
              --                             ""
              --                             companyPrefix
              --                             "userTests First Name Empty Password"
              --                             "userTests Last Name Empty Password"
              --                             "userTests Phone Number Empty Password"

          -- Create a business to use from further test cases (this is tested in
          -- the businesses tests so doesn't need to be explicitly tested here).
          _ <- http (addBusiness brAuthUser business)

          -- Add good RSA Public Key for using from the test cases.
          Just goodKey <- goodRsaPublicKey

          -- We delibrately test the "good user" that we will later add so that
          -- we know that we are failing because they aren't in the DB rather
          -- then because they are somehow otherwise invalid.
          step "That a user that doesn't exist can't login"
          nonExistantUserResult <- http (addPublicKey (newUserToBasicAuthData user1) goodKey Nothing)
          nonExistantUserResult `shouldSatisfy` isLeft
          nonExistantUserResult `shouldSatisfy` (checkFailureStatus NS.unauthorized401)
          nonExistantUserResult `shouldSatisfy` (checkFailureMessage "")

          step "Can create a new user"
          http (addUser brAuthUser user1)
            `shouldSatisfyIO` isRight
          -- Note: We effectively implicitly test that the value returned is
          --       sensible later when we test that a user with this ID occurs
          --       in a keys query response, here we can only test that we think
          --       that we succeeded and have no other way of verifying the ID
          --       is otherwise correct constraining our selves to just user
          --       related API functions.

          step "That the created user can login"
          http (addPublicKey (newUserToBasicAuthData user1) goodKey Nothing)
            `shouldSatisfyIO` isRight
          -- Note: We test the result of the function elsewhere, all we care
          --       about here is that the user can login.

          step "That the wrong password doesn't allow the user to login"
          wrongPasswordResult <- http (addPublicKey (newUserToBasicAuthData user1){basicAuthPassword = "invalid password"} goodKey Nothing)
          wrongPasswordResult `shouldSatisfy` isLeft
          wrongPasswordResult `shouldSatisfy` (checkFailureStatus NS.unauthorized401)
          wrongPasswordResult `shouldSatisfy` (checkFailureMessage "")

          step "That the an empty password doesn't allow the user to login"
          emptyPasswordResult <- http (addPublicKey (newUserToBasicAuthData user1){basicAuthPassword = ""} goodKey Nothing)
          emptyPasswordResult `shouldSatisfy` isLeft
          emptyPasswordResult `shouldSatisfy` (checkFailureStatus NS.unauthorized401)
          emptyPasswordResult `shouldSatisfy` (checkFailureMessage "")

          step "Can't create a new user with a GS1CompanyPrefix that isn't registered"
          invalidPrefixResult <- http (addUser brAuthUser userNonRegisteredBiz)
          invalidPrefixResult `shouldSatisfy` isLeft
          invalidPrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          invalidPrefixResult `shouldSatisfy` (checkFailureMessage "Business does not exist.")

          step "Can't create a new user with the same email address"
          duplicateEmailResult <- http (addUser brAuthUser userSameEmail)
          duplicateEmailResult `shouldSatisfy` isLeft
          duplicateEmailResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          duplicateEmailResult `shouldSatisfy` (checkFailureMessage "Unable to create user.")

          step "Can create a second user"
          http (addUser brAuthUser user2)
            `shouldSatisfyIO` isRight

          -- TODO: Include me (github #205):
          -- step "Can't create a user with an empty email."
          -- emptyEmailResult <- http (addUser brAuthUser userEmptyEmail)
          -- emptyEmailResult `shouldSatisfy` isLeft
          -- emptyEmailResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- emptyEmailResult `shouldSatisfy` (checkFailureMessage "TODO")

          -- -- TODO: Include me (github #205):
          -- step "Can't create a user with an empty password."
          -- emptyUserPasswordResult <- http (addUser brAuthUser userEmptyPassword)
          -- emptyUserPasswordResult `shouldSatisfy` isLeft
          -- emptyUserPasswordResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- emptyUserPasswordResult `shouldSatisfy` (checkFailureMessage "TODO")


  let keyTests = testCaseSteps "That keys work as expected" $ \step ->
        bracket runBRApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, brAuthUser) -> do
          password <- randomPassword
          let http = runClient baseurl
              biz1Prefix = (GS1CompanyPrefix "4000001")
              biz1 = NewBusiness biz1Prefix "userTests_businessName1"
              biz2Prefix = (GS1CompanyPrefix "4000002")
              biz2 = NewBusiness biz2Prefix "userTests_businessName2"

          -- Business1User1
          let userB1U1 = NewUser (unsafeMkEmailAddress "keysTests_email1@example.com")
                                 password
                                 biz1Prefix
                                 "keysTests First Name 1"
                                 "keysTests Last Name 1"
                                 "keysTests Phone Number 1"
          -- Business1User2
          let userB1U2 = NewUser (unsafeMkEmailAddress "keysTests_email2@example.com")
                                 password
                                 biz1Prefix
                                 "keysTests First Name 2"
                                 "keysTests Last Name 2"
                                 "keysTests Phone Number 2"
          -- Business2User1
          let userB2U1 = NewUser (unsafeMkEmailAddress "keysTests_email3@example.com")
                                 password
                                 biz2Prefix
                                 "keysTests First Name 3"
                                 "keysTests Last Name 3"
                                 "keysTests Phone Number 3"

          -- Create a business to use from further test cases (this is tested in
          --  the businesses tests so doesn't need to be explicitly tested here).
          _ <- http (addBusiness brAuthUser biz1)
          _ <- http (addBusiness brAuthUser biz2)

          -- Create a business to use from further test cases (this is tested in
          -- the businesses tests so doesn't need to be explicitly tested here).
          userB1U1Response <- http (addUser brAuthUser userB1U1)
          _                <- http (addUser brAuthUser userB1U2)
          _                <- http (addUser brAuthUser userB2U1)

          -- Add good RSA Public Key for using from the test cases.
          Just goodKey <- goodRsaPublicKey

          step "Can add a good key (no exipry time)"
          b1K1PreInsertionTime <- getCurrentTime
          b1K1StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          b1K1PostInsertionTime <- getCurrentTime
          b1K1StoredKeyIdResult `shouldSatisfy` isRight

          let Right b1K1StoredKeyId = b1K1StoredKeyIdResult

          step "Can retrieve a stored key"
          b1K1Response <- http (getPublicKey b1K1StoredKeyId)
          b1K1Response `shouldSatisfy` isRight

          step "Can retrieve the key info for a stored key"
          b1K1InfoResponse <- http (getPublicKeyInfo b1K1StoredKeyId)
          b1K1InfoResponse `shouldSatisfy` isRight
          let KeyInfoResponse
                ky1InfoId
                ky1InfoUserId
                ky1InfoState
                ky1InfoCreationTime
                ky1InfoRevocationTime
                ky1InfoExpirationTime
                ky1InfoPEMString
                = fromRight b1K1InfoResponse
          ky1InfoId             `shouldSatisfy` (== b1K1StoredKeyId)
          ky1InfoUserId         `shouldSatisfy` (== fromRight userB1U1Response)
          ky1InfoState          `shouldSatisfy` (== InEffect)
          ky1InfoRevocationTime `shouldSatisfy` isNothing
          ky1InfoExpirationTime `shouldSatisfy` isNothing
          ky1InfoPEMString      `shouldSatisfy` (== goodKey)
          getCreationTime ky1InfoCreationTime
            `shouldSatisfy` (betweenInclusive b1K1PreInsertionTime b1K1PostInsertionTime)

          step "That getPublicKey fails gracefully searching for a non existant key"
          b1InvalidKeyResponse <- http (getPublicKey (BRKeyId nil))
          b1InvalidKeyResponse `shouldSatisfy` isLeft
          b1InvalidKeyResponse `shouldSatisfy` (checkFailureStatus NS.notFound404)
          b1InvalidKeyResponse `shouldSatisfy` (checkFailureMessage "Public key with the given id not found.")

          step "That getPublicKeyInfo fails gracefully searching for a non existant key"
          b1InvalidKeyInfoResponse <- http (getPublicKeyInfo (BRKeyId nil))
          b1InvalidKeyInfoResponse `shouldSatisfy` isLeft
          b1InvalidKeyInfoResponse `shouldSatisfy` (checkFailureStatus NS.notFound404)
          b1InvalidKeyInfoResponse `shouldSatisfy` (checkFailureMessage "Public key with the given id not found.")

          let expiryDelay = 3
          step $ "Can add a good key with exipry time (" ++ (show expiryDelay) ++ " seconds from now)"
          b1K2Expiry <- (Just . ExpirationTime) <$> ((addUTCTime (fromInteger expiryDelay)) <$> getCurrentTime)
          b1K2StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey b1K2Expiry)
          b1K2StoredKeyIdResult `shouldSatisfy` isRight

          let Right b1K2StoredKeyId = b1K2StoredKeyIdResult

          step "That the key info reflects the expiry time"
          b1K2InfoResponse <- http (getPublicKeyInfo b1K2StoredKeyId)
          b1K2InfoResponse `shouldSatisfy` isRight
          let KeyInfoResponse
                ky2InfoId
                ky2InfoUserId
                ky2InfoState
                _ky2InfoCreationTime
                ky2InfoRevocationTime
                ky2InfoExpirationTime
                ky2InfoPEMString
                = fromRight b1K2InfoResponse
          ky2InfoId             `shouldSatisfy` (== b1K2StoredKeyId)
          ky2InfoUserId         `shouldSatisfy` (== fromRight userB1U1Response)
          ky2InfoState          `shouldSatisfy` (== InEffect)
          ky2InfoRevocationTime `shouldSatisfy` isNothing
          ky2InfoPEMString      `shouldSatisfy` (== goodKey)
          getExpirationTime (fromJust ky2InfoExpirationTime)
            `shouldSatisfy` within1Second ((getExpirationTime . fromJust) b1K2Expiry)

          step "That the key info status updates after the expiry time has been reached"
          threadDelay $ fromIntegral $ secondsToMicroseconds expiryDelay
          b1K2InfoDelayedResponse <- http (getPublicKeyInfo b1K2StoredKeyId)
          b1K2InfoDelayedResponse `shouldSatisfy` isRight
          b1K2InfoDelayedResponse `shouldSatisfy` checkField keyInfoState (== Expired)

          step "Test that it is not possible to revoke a key that has already expired."
          b1K2RevokedResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K2StoredKeyId)
          b1K2RevokedResponse `shouldSatisfy` isLeft
          b1K2RevokedResponse `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          b1K2RevokedResponse `shouldSatisfy` (checkFailureMessage "Public key already expired.")

          step "That it is not possible to add a key that is already expired"
          b1ExpiredKeyExpiry <- (Just . ExpirationTime) <$> ((addUTCTime (fromInteger (-1))) <$> getCurrentTime)
          b1ExpiredKeyExpiryResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey b1ExpiredKeyExpiry)
          b1ExpiredKeyExpiryResult `shouldSatisfy` isLeft
          b1ExpiredKeyExpiryResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          b1ExpiredKeyExpiryResult `shouldSatisfy` (checkFailureMessage "Can't add a key that has already expired.")

          step "That it's possible to revoke a key"
          b1K3StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          b1K3StoredKeyIdResult `shouldSatisfy` isRight
          let b1K3StoredKeyId = fromRight b1K3StoredKeyIdResult
          b1K3PreRevoke <- getCurrentTime
          b1K3RevokedResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K3StoredKeyId)
          b1K3PostRevoke <- getCurrentTime
          b1K3RevokedResponse `shouldSatisfy` isRight
          b1K3RevokedResponse `shouldSatisfy` checkField getRevocationTime (betweenInclusive b1K3PreRevoke b1K3PostRevoke)

          step "That the key info correctly shows the revokation status time and revoking user"
          let extractRevocationTime = getRevocationTime . fst . fromJust
          b1K3InfoResponse <- http (getPublicKeyInfo b1K3StoredKeyId)
          b1K3InfoResponse `shouldSatisfy` isRight
          b1K3InfoResponse `shouldSatisfy` checkField keyInfoRevocation isJust
          b1K3InfoResponse `shouldSatisfy` checkField keyInfoRevocation ((betweenInclusive b1K3PreRevoke b1K3PostRevoke) . extractRevocationTime)
          b1K3InfoResponse `shouldSatisfy` checkField keyInfoRevocation ((== fromRight userB1U1Response) . snd . fromJust)
          -- We check that the time through this responce ~matches the time that was given when we revoked the key.
          let b1K3RevokedResponseTime = (getRevocationTime . fromRight) b1K3RevokedResponse
          b1K3InfoResponse `shouldSatisfy` checkField keyInfoRevocation ((within1Second b1K3RevokedResponseTime) . extractRevocationTime)

          step "That the key status updates after the key is revoked"
          b1K3RevokedInfoResponse <- http (getPublicKeyInfo b1K3StoredKeyId)
          b1K3RevokedInfoResponse `shouldSatisfy` isRight
          b1K3RevokedInfoResponse `shouldSatisfy` checkField keyInfoState (== Revoked)

          step "That revoking an already revoked key generates an error"
          b1K3RevokedAgainResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K3StoredKeyId)
          b1K3RevokedAgainResponse `shouldSatisfy` isLeft
          b1K3RevokedAgainResponse `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          b1K3RevokedAgainResponse `shouldSatisfy` (checkFailureMessage "Public key already revoked.")

          -- TODO: Include this test. (github #211)
          -- step "That another user from the same business can also revoke the key"
          -- b1K4StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          -- b1K4StoredKeyIdResult `shouldSatisfy` isRight
          -- let b1K4StoredKeyId = fromRight b1K4StoredKeyIdResult
          -- b1K4RevokedResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U2) b1K4StoredKeyId)
          -- b1K4RevokedResponse `shouldSatisfy` isRight
          -- b1K4RevokedInfoResponse <- http (getPublicKeyInfo b1K4StoredKeyId)
          -- b1K4RevokedInfoResponse `shouldSatisfy` isRight
          -- b1K4RevokedInfoResponse `shouldSatisfy` (checkField keyInfoState (== Revoked))
          --TODO: Also check that the revoking user is correct and is different from the original adding user.

          step "That a user from the another business can't also revoke the key"
          b1K5StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          b1K5StoredKeyIdResult `shouldSatisfy` isRight
          let Right b1K5StoredKeyId = b1K5StoredKeyIdResult
          b1K5RevokedResponse <- http (revokePublicKey (newUserToBasicAuthData userB2U1) b1K5StoredKeyId)
          b1K5RevokedResponse `shouldSatisfy` isLeft
          b1K5RevokedResponse `shouldSatisfy` (checkFailureStatus NS.forbidden403)
          b1K5RevokedResponse `shouldSatisfy` (checkFailureMessage "Not authorised to access this key.")
          b1K5RevokedInfoResponse <- http (getPublicKeyInfo b1K5StoredKeyId)
          b1K5RevokedInfoResponse `shouldSatisfy` isRight
          b1K5RevokedInfoResponse `shouldSatisfy` checkField keyInfoState (== InEffect)

          step "That revokePublicKey for an invalid keyId fails gracefully"
          revokeInvalidKeyIdResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) (BRKeyId nil))
          revokeInvalidKeyIdResponse `shouldSatisfy` isLeft
          revokeInvalidKeyIdResponse `shouldSatisfy` (checkFailureStatus NS.notFound404)
          revokeInvalidKeyIdResponse `shouldSatisfy` (checkFailureMessage "Public key with the given id not found.")

          step "Test where the key has an expiry time (which hasn't expired) and is revoked reports the correct status."
          b1K6ExpiryUTC <- (addUTCTime (fromInteger expiryDelay)) <$> getCurrentTime
          let b1K6Expiry = Just . ExpirationTime $ b1K6ExpiryUTC
          b1K6StoreKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey b1K6Expiry)
          b1K6StoreKeyIdResult `shouldSatisfy` isRight
          let Right b1K6KeyId = b1K6StoreKeyIdResult
          b1K6RevokeKeyResult <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K6KeyId)
          b1K6RevokeKeyResult `shouldSatisfy` isRight
          b1K6ExpiryRevokedResponse <- http (getPublicKeyInfo b1K6KeyId)
          -- This a test integrity check. We need to make sure that the time
          -- after we sent the request is not enough that the key will be after exipry time here.
          b1K6TimeAfterResponse <- getCurrentTime
          b1K6TimeAfterResponse `shouldSatisfy` (< b1K6ExpiryUTC)
          b1K6ExpiryRevokedResponse `shouldSatisfy` isRight
          b1K6ExpiryRevokedResponse `shouldSatisfy` checkField keyInfoState (== Revoked)

          step "Test where the key has an expiry time and a revoked time which expired after it was revoked and both revoked and expired time have passed."
          -- Wait for the key from the previous test to expire and then recheck the status.
          threadDelay $ secondsToMicroseconds $ ceiling $ diffUTCTime b1K6ExpiryUTC b1K6TimeAfterResponse
          b1K6ExpiredRevokedResponse <- http (getPublicKeyInfo b1K6KeyId)
          b1K6ExpiredRevokedResponse `shouldSatisfy` isRight
          b1K6ExpiredRevokedResponse `shouldSatisfy` checkField keyInfoState (== Revoked)


          -- Function to run a test predicate over all the keys in one of the test keys subdirectories.
          let testDirectory keyDirectory suffix predicate = do
                let directory = "test" </> "Mirza" </> "Common" </> "TestData" </> "testKeys" </> keyDirectory
                files <- filter (suffix `isSuffixOf`) <$> listDirectory directory
                let fullyQualifiedFiles = (directory </>) <$> files
                keys <- traverse readJWK fullyQualifiedFiles
                forM_ (zip files keys) $ \(keyName,Just key) -> do
                  step $ "Testing " ++ keyDirectory ++ " key: " ++ keyName
                  http (addPublicKey (newUserToBasicAuthData userB1U1) key Nothing)
                    `shouldSatisfyIO` predicate

          step "Can add all of the good keys"
          testDirectory "goodJWKs" "_pub.json" isRight
          -- Note: 2041 / 2047 bit key tests have been moved to the good tests because we know that there is an issue
          -- with the implementation here based on limitiations of the library that we are using and have accepted that
          -- we will allow keys with this size to be supported even though we hope that our users will only use keys
          -- with size 2048 and above. See github issue #212
          -- https://github.csiro.au/Blockchain/supplyChainServer/issues/212#issuecomment-13326 for further information.

          step "Can't add any of the bad keys"
          -- The private keys from the goodJWKs dirctory are also bad keys
          testDirectory "goodJWKs" "rsa.json" isLeft
          testDirectory "badJWKs" "rsa.json" isLeft
          testDirectory "badJWKs" "_pub.json" isLeft


  let locationTests = testCaseSteps "That locations work as expected" $ \step ->
        bracket runBRApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, brAuthUser) -> do
          password <- randomPassword
          let http = runClient baseurl
              biz1Prefix = (GS1CompanyPrefix "5000001")
              biz1 = NewBusiness biz1Prefix "locationTests_businessName1"
              biz2Prefix = (GS1CompanyPrefix "5000002")
              biz2 = NewBusiness biz2Prefix "locationTests_businessName2"

          -- Business1User1
          let userB1U1 = NewUser (unsafeMkEmailAddress "locationTests_email1@example.com")
                                password
                                biz1Prefix
                                "locationTests First Name 1"
                                "locationTests Last Name 1"
                                "locationTests Phone Number 1"
          _ <- http (addBusiness brAuthUser biz1)
          _ <- http (addBusiness brAuthUser biz2)

          _userB1U1Response <- http (addUser brAuthUser userB1U1)


          step "Can Add a Location"
          let newLoc1 = NewLocation (SGLN biz1Prefix (LocationReference "98765") Nothing)
                                    (Just (Latitude 1.0, Longitude 2.0))
                                    (Just "42 Wallby Way, Sydney")
          b1K1StoredKeyIdResult <- http (addLocation brAuthUser newLoc1)
          b1K1StoredKeyIdResult `shouldSatisfy` isRight


  let healthTests = testCaseSteps "Provides health status" $ \step ->
        bracket runBRApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, _brAuthUser) -> do
          let http = runClient baseurl

          step "Status results in 200"
          healthResult <- http health
          healthResult `shouldSatisfy` isRight
          healthResult `shouldBe` (Right HealthResponse)



  pure $ testGroup "Business Registry HTTP Client tests"
        [ businessTests
        , userTests
        , keyTests
        , locationTests
        , healthTests
        ]

-- Test helper function that enables a predicate to be run on the result of a
-- test call.
checkField :: (a -> b) -> (b -> Bool) -> Either c a -> Bool
checkField accessor predicate = either (const False) (predicate . accessor)

checkFailureStatus :: NS.Status -> Either ServantError a -> Bool
checkFailureStatus = checkFailureField responseStatusCode

checkFailureMessage :: ByteString -> Either ServantError a -> Bool
checkFailureMessage = checkFailureField responseBody

checkFailureField :: (Eq a) => (Response -> a) -> a -> Either ServantError b -> Bool
checkFailureField accessor x (Left (FailureResponse failure)) = x == (accessor failure)
checkFailureField _        _ _                                = False
