{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Mirza.OrgRegistry.Tests.Client where

import           Control.Concurrent                       (threadDelay)
import           Control.Exception                        (bracket)

import           Mirza.Common.Tests.ServantUtils
import           Mirza.OrgRegistry.GenerateUtils (dummyOrg, dummyUser)
import           Mirza.Common.Utils

import           Servant.API.BasicAuth
import           Servant.Client

import           Network.URI                              (URI (..), URIAuth (..), nullURI)

import           Data.ByteString.Lazy                     (ByteString)

import           System.Directory                         (listDirectory)
import           System.FilePath                          ((</>))

import           Control.Monad                            (forM_)
import           Data.Either                              (isLeft, isRight)
import           Data.Either.Utils                        (fromRight)
import           Data.Function                            ((&))
import           Data.List                                (isSuffixOf)
import           Data.Maybe                               (fromJust, isJust,
                                                           isNothing)
import           Data.Time.Clock                          (addUTCTime,
                                                           diffUTCTime,
                                                           getCurrentTime)
import           Data.UUID                                (nil)

import qualified Network.HTTP.Types.Status                as NS

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.GS1.EPC                             (GS1CompanyPrefix (..),
                                                           LocationEPC (SGLN),
                                                           LocationReference (LocationReference))

import           Mirza.OrgRegistry.Client.Servant
import           Mirza.OrgRegistry.Types             hiding (orgName)

import           Mirza/OrgRegistry/Handlers/Organisation (orgToOrgResponse,
                                                           newOrgToOrg)

import           Mirza.Common.Time
import           Mirza.Common.Utils                       (readJWK)

import           Mirza.OrgRegistry.Tests.Utils
import           Mirza.Common.Tests.InitClient
import           Mirza.Common.Tests.Utils

-- === OR Servant Client tests
clientSpec :: IO TestTree
clientSpec = do
  let orgTests = testCaseSteps "Can create orgs" $ \step ->
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid,baseurl,orAuthUser) -> do
          let http = runClient baseurl
              org1Prefix   = GS1CompanyPrefix "2000001"
              org1Name     = "orgTests_org1Name"
              org1         = NewOrg org1Prefix org1Name (mockURI org1Name)
              org1Response = newOrgToOrgResponse org1
              org2Prefix   = GS1CompanyPrefix "2000002"
              org2Name     = "orgTests_org2Name"
              org2         =  NewOrg org2Prefix org2Name (mockURI org2Name)
              org2Response = newOrgToOrgResponse org2
              org3Prefix   = GS1CompanyPrefix "3000003"
              org3Name     = "A strange name"
              org3         =  NewOrg org3Prefix org3Name (mockURI org3Name)
              org3Response = newOrgToOrgResponse org3
              -- emptyPrefixOrg = NewOrg (GS1CompanyPrefix "") "EmptyOrg"
              -- stringPrefix1Org = NewOrg (GS1CompanyPrefix "string") "EmptyOrg"

          step "Can create a new org"
          addOrg1Result <- http (addOrg orAuthUser org1)
          addOrg1Result `shouldSatisfy` isRight
          addOrg1Result `shouldBe` (Right org1Prefix)


          step "That the added org was added and can be listed."
          http (searchOrgs Nothing Nothing Nothing) >>=
            either (const $ expectationFailure "Error listing orgs")
                   (`shouldContain` [org1Response])

          step "Can't add org with the same GS1CompanyPrefix"
          duplicatePrefixResult <- http (addOrg orAuthUser org1{newOrgName = "orgTests_anotherName"})
          duplicatePrefixResult `shouldSatisfy` isLeft
          duplicatePrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          duplicatePrefixResult `shouldSatisfy` (checkFailureMessage "GS1 company prefix already exists.")

          step "Can add a second org"
          addOrg2Result <- http (addOrg orAuthUser org2)
          addOrg2Result `shouldSatisfy` isRight
          addOrg2Result `shouldBe` (Right org2Prefix)

          step "List orgs returns all of the orgs"
          http (searchOrgs Nothing Nothing Nothing) >>=
              either (const $ expectationFailure "Error listing orgs")
                    (`shouldContain` [ org1Response
                                        , org2Response])

          step "Can add a third org"
          addOrg3Result <- http (addOrg orAuthUser org3)
          addOrg3Result `shouldSatisfy` isRight
          addOrg3Result `shouldBe` (Right org3Prefix)

          step "Searching by GS1 ID works"
          searchOrg3Result <- http (searchOrgs (Just org3Prefix) Nothing Nothing)
          searchOrg3Result `shouldSatisfy` isRight
          searchOrg3Result `shouldBe` (Right [org3Response])

          step "Searching by org name works"
          searchOrg3NameResult <- http (searchOrgs Nothing (Just "strange") Nothing)
          searchOrg3NameResult `shouldSatisfy` isRight
          searchOrg3NameResult `shouldBe` (Right [org3Response])

          searchOrg12NameResult <- http (searchOrgs Nothing (Just "Tests_") Nothing)
          searchOrg12NameResult `shouldSatisfy` isRight
          searchOrg12NameResult & either (error "You said this was Right!")
                                         (`shouldContain` [ org1Response
                                                          , org2Response])

          -- TODO: Include me (github #205):
          -- step "That the GS1CompanyPrefix can't be empty (\"\")."
          -- emptyPrefixResult <- http (addOrg orAuthUser emptyPrefixOrg)
          -- emptyPrefixResult `shouldSatisfy` isLeft
          -- emptyPrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- emptyPrefixResult `shouldSatisfy` (checkFailureMessage "TODO")

          -- TODO: Include me (github #205):
          -- This should possibly be changed to something that is within the
          -- type specification but is logically incorrect if the type
          -- constraint is improved.
          -- step "That the GS1CompanyPrefix can't be a string."
          -- stringPrefixResult <- http (addOrg orAuthUser stringPrefix1Org)
          -- stringPrefixResult `shouldSatisfy` isLeft
          -- stringPrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- stringPrefixResult `shouldSatisfy` (checkFailureMessage "TODO")

          step "Can't add org with a nullUIL"
          nullURLResult <- http (addOrg orAuthUser org1{newOrgUrl = nullURI})
          nullURLResult `shouldSatisfy` isLeft
          nullURLResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          nullURLResult `shouldSatisfy` (checkFailureMessage "Error in $.newOrgUrl: not a URI")

          step "Can't add org with an invalid URL"
          invalidURLResult <- http (addOrg orAuthUser org1{newOrgUrl = URI "" (Just $ URIAuth "" "invalid" "") "" "" ""})
          invalidURLResult `shouldSatisfy` isLeft
          invalidURLResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          invalidURLResult `shouldSatisfy` (checkFailureMessage "Error in $.newOrgUrl: not a URI")


  let userTests = testCaseSteps "Can create users" $ \step ->
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid,baseurl,orAuthUser) -> do
          password <- randomPassword

          let http = runClient baseurl
              companyPrefix = (GS1CompanyPrefix "3000001")
              orgName = "userTests_orgName"
              org = NewOrg companyPrefix orgName (mockURI orgName)

          let user1 = NewUser "OAuthSub_userTests_email1"
                              (unsafeMkEmailAddress "userTests_email1@example.com")
                              password
                              companyPrefix
                              "userTests First Name 1"
                              "userTests Last Name 1"
                              "userTests Phone Number 1"
              user2 = NewUser "OAuthSub_userTests_email2"
                              (unsafeMkEmailAddress "userTests_email2@example.com")
                              password
                              companyPrefix
                              "userTests First Name 2"
                              "userTests Last Name 2"
                              "userTests Phone Number 2"
              -- Same email address as user1 other fields different.
              userSameEmail = NewUser "OAuthSub_userTests_same_email"
                                      (newUserEmailAddress user1)
                                      password
                                      companyPrefix
                                      "userTests First Name Same Email"
                                      "userTests Last Name Same Email"
                                      "userTests Phone Number Same Email"
              userNonRegisteredOrg = NewUser "OAuthSub_userTests_unregistered"
                                             (unsafeMkEmailAddress "userTests_unregisteredOrg@example.com")
                                             password
                                             (GS1CompanyPrefix "unregistered")
                                             "userTests First Name Unregistered Org"
                                             "userTests Last Name Unregistered Org"
                                             "userTests Phone Number Unregistered Org"
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

          -- Create a org to use from further test cases (this is tested in
          -- the orgs tests so doesn't need to be explicitly tested here).
          _ <- http (addOrg orAuthUser org)

          -- Add good RSA Public Key for using from the test cases.
          Just goodKey <- goodRsaPublicKey


          -- We delibrately test the "good user" that we will later add so that
          -- we know that we are failing because they aren't in the DB rather
          -- then because they are somehow otherwise invalid.
          step "That a user that doesn't exist can't login"
          nonExistentUserResult <- http (addPublicKey (newUserToBasicAuthData user1) goodKey Nothing)
          nonExistentUserResult `shouldSatisfy` isLeft
          nonExistentUserResult `shouldSatisfy` (checkFailureStatus NS.unauthorized401)
          nonExistentUserResult `shouldSatisfy` (checkFailureMessage "")

          step "Can create a new user"
          http (addUser orAuthUser user1)
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
          invalidPrefixResult <- http (addUser orAuthUser userNonRegisteredOrg)
          invalidPrefixResult `shouldSatisfy` isLeft
          invalidPrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          invalidPrefixResult `shouldSatisfy` (checkFailureMessage "Org does not exist.")

          step "Can't create a new user with the same email address"
          duplicateEmailResult <- http (addUser orAuthUser userSameEmail)
          duplicateEmailResult `shouldSatisfy` isLeft
          duplicateEmailResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          duplicateEmailResult `shouldSatisfy` (checkFailureMessage "Unable to create user.")

          step "Can create a second user"
          http (addUser orAuthUser user2)
            `shouldSatisfyIO` isRight

          step "/company returns the correct company"
          orgSearchRes <- http (getOrgInfo (newUserToBasicAuthData user1))
          orgSearchRes `shouldSatisfy` isRight
          let (Right [user1Org]) = orgSearchRes
          (orgToOrgResponse . newOrgToOrg $ org)
              `shouldBe`
                  user1Org

          -- TODO: Include me (github #205):
          -- step "Can't create a user with an empty email."
          -- emptyEmailResult <- http (addUser orAuthUser userEmptyEmail)
          -- emptyEmailResult `shouldSatisfy` isLeft
          -- emptyEmailResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- emptyEmailResult `shouldSatisfy` (checkFailureMessage "TODO")

          -- -- TODO: Include me (github #205):
          -- step "Can't create a user with an empty password."
          -- emptyUserPasswordResult <- http (addUser orAuthUser userEmptyPassword)
          -- emptyUserPasswordResult `shouldSatisfy` isLeft
          -- emptyUserPasswordResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- emptyUserPasswordResult `shouldSatisfy` (checkFailureMessage "TODO")


  let keyTests = testCaseSteps "That keys work as expected" $ \step ->
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, orAuthUser) -> do
          password <- randomPassword
          let http = runClient baseurl
              org1Prefix = (GS1CompanyPrefix "4000001")
              org1Name = "userTests_orgName1"
              org1 = NewOrg org1Prefix org1Name (mockURI org1Name)
              org2Prefix = (GS1CompanyPrefix "4000002")
              org2Name = "userTests_orgName2"
              org2 = NewOrg org2Prefix org2Name (mockURI org2Name)

          -- Org1User1
          let userB1U1 = NewUser "OAuthSub_keysTests_email1"
                                 (unsafeMkEmailAddress "keysTests_email1@example.com")
                                 password
                                 org1Prefix
                                 "keysTests First Name 1"
                                 "keysTests Last Name 1"
                                 "keysTests Phone Number 1"
          -- Org1User2
          let userB1U2 = NewUser "OAuthSub_keysTests_email2"
                                 (unsafeMkEmailAddress "keysTests_email2@example.com")
                                 password
                                 org1Prefix
                                 "keysTests First Name 2"
                                 "keysTests Last Name 2"
                                 "keysTests Phone Number 2"
          -- Org2User1
          let userB2U1 = NewUser "OAuthSub_keysTests_email3"
                                 (unsafeMkEmailAddress "keysTests_email3@example.com")
                                 password
                                 org2Prefix
                                 "keysTests First Name 3"
                                 "keysTests Last Name 3"
                                 "keysTests Phone Number 3"

          -- Create a org to use from further test cases (this is tested in
          --  the orgs tests so doesn't need to be explicitly tested here).
          _ <- http (addOrg orAuthUser org1)
          _ <- http (addOrg orAuthUser org2)

          -- Create a user to use from further test cases (this is tested in
          -- the users tests so doesn't need to be explicitly tested here).
          userB1U1Response <- http (addUser orAuthUser userB1U1)
          _                <- http (addUser orAuthUser userB1U2)
          _                <- http (addUser orAuthUser userB2U1)

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
          b1InvalidKeyResponse <- http (getPublicKey (ORKeyId nil))
          b1InvalidKeyResponse `shouldSatisfy` isLeft
          b1InvalidKeyResponse `shouldSatisfy` (checkFailureStatus NS.notFound404)
          b1InvalidKeyResponse `shouldSatisfy` (checkFailureMessage "Public key with the given id not found.")

          step "That getPublicKeyInfo fails gracefully searching for a non existant key"
          b1InvalidKeyInfoResponse <- http (getPublicKeyInfo (ORKeyId nil))
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
          -- step "That another user from the same org can also revoke the key"
          -- b1K4StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          -- b1K4StoredKeyIdResult `shouldSatisfy` isRight
          -- let b1K4StoredKeyId = fromRight b1K4StoredKeyIdResult
          -- b1K4RevokedResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U2) b1K4StoredKeyId)
          -- b1K4RevokedResponse `shouldSatisfy` isRight
          -- b1K4RevokedInfoResponse <- http (getPublicKeyInfo b1K4StoredKeyId)
          -- b1K4RevokedInfoResponse `shouldSatisfy` isRight
          -- b1K4RevokedInfoResponse `shouldSatisfy` (checkField keyInfoState (== Revoked))
          --TODO: Also check that the revoking user is correct and is different from the original adding user.

          step "That a user from the another org can't also revoke the key"
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
          revokeInvalidKeyIdResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) (ORKeyId nil))
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
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, orAuthUser) -> do
          let http = runClient baseurl
              org1Gen = dummyOrg "locationTests_Org1"
              org1 = org1Gen {newOrgGS1CompanyPrefix = (GS1CompanyPrefix "5000001")} -- todo fix this properly...
              org1Prefix = newOrgGS1CompanyPrefix org1
              org2Gen = dummyOrg "locationTests_Org2"
              org2 = org2Gen {newOrgGS1CompanyPrefix = (GS1CompanyPrefix "5000002")} -- todo fix this properly...
              org2Prefix = newOrgGS1CompanyPrefix org2
              org3Gen = dummyOrg "locationTests_Org3"
              org3 = org3Gen {newOrgGS1CompanyPrefix = (GS1CompanyPrefix "5000003")} -- todo fix this properly...
              org3Prefix = newOrgGS1CompanyPrefix org3

          -- Create a org to use from further test cases (this is tested in
          --  the orgs tests so doesn't need to be explicitly tested here).
          _ <- http (addOrg orAuthUser org1)
          _ <- http (addOrg orAuthUser org2)
          _ <- http (addOrg orAuthUser org3)


          -- Org1User1
          userB1U1 <- dummyUser "locationTests_Org1User1" org1Prefix
          -- Org2User1
          userB2U1 <- dummyUser "locationTests_Org2User1" org2Prefix
          -- Org3User1
          userB3U1 <- dummyUser "locationTests_Org3User1" org3Prefix

          -- Create a user to use from further test cases (this is tested in
          -- the users tests so doesn't need to be explicitly tested here).
          _userB1U1Response <- http (addUser orAuthUser userB1U1)
          _userB1U2Response <- http (addUser orAuthUser userB2U1)
          _userB1U3Response <- http (addUser orAuthUser userB3U1)


          step "Can add a location"
          let location1 = NewLocation (SGLN org1Prefix (LocationReference "00011") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation1Result <- http (addLocation (newUserToBasicAuthData userB1U1) location1)
          addLocation1Result `shouldSatisfy` isRight


          -- step "TODO: Can't add a location for a company that doesn't exist."
          -- let nonExistantCompanyPrefix = (GS1CompanyPrefix "5999999")
          --     location2 = NewLocation (SGLN nonExistantCompanyPrefix (LocationReference "00013") Nothing)
          --                             (Just (Latitude (-25.344490), Longitude 131.035431))
          --                             (Just "42 Wallby Way, Sydney")
          -- addLocation2Result1 <- http (addLocation orAuthUser location2)
          -- addLocation2Result1 `shouldSatisfy` isLeft
          -- addLocation2Result2 <- http (addLocation (newUserToBasicAuthData userB1U1) location2)
          -- addLocation2Result2 `shouldSatisfy` isLeft


          step "That a user can only insert a location from their busniess."
          let location3 = NewLocation (SGLN org2Prefix (LocationReference "00017") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation3Result <- http (addLocation (newUserToBasicAuthData userB1U1) location3)
          addLocation3Result `shouldSatisfy` isLeft


          step "Can add a second location with a different company."
          addLocation4Result <- http (addLocation (newUserToBasicAuthData userB2U1) location3)
          addLocation4Result `shouldSatisfy` isRight


          step "Can add a second location with the same company."
          let location5 = NewLocation (SGLN org1Prefix (LocationReference "00019") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation5Result <- http (addLocation (newUserToBasicAuthData userB1U1) location5)
          addLocation5Result `shouldSatisfy` isRight


          step "Can't add a location with a duplicate LocationReference for the same org."
          let location6 = NewLocation (SGLN org1Prefix (LocationReference "00019") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation6Result <- http (addLocation (newUserToBasicAuthData userB1U1) location6)
          addLocation6Result `shouldSatisfy` isLeft


          step "Can add a location with a duplicate LocationReference for different orgs."
          let location7 = NewLocation (SGLN org3Prefix (LocationReference "00019") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation7Result <- http (addLocation (newUserToBasicAuthData userB3U1) location7)
          addLocation7Result `shouldSatisfy` isRight


          -- step "TODO: Can search for a location based on GLN."

          -- step "TODO: Can serach for a location based on a GS1 company prefix."
          --   Search for 1 of the locations with the exclusion of the other.
          --   Serach fot the other location with the exclusion of the first.
          --   (A serach the returns multiple results (should be part of one of the above 2 queries).

          -- step "TODO: Can serach for a location based on modified since field."

          -- step "TODO: uxLocation: Can query all of the locations associated with a org"
          -- step "TODO: uxLocation: Can query all of the locations associated with multiple org"
          -- step "TODO: uxLocation: That quering for a non existant org results in an empty result"
          -- step "TODO: uxLocation: That quering for a non existant org in addition to multiple orgs just ignores the non existant org"
          -- step "TODO: uxLocation: That quering for locations from more then 25 orgs ignores the orgs beyond 25 are ignored."



  let healthTests = testCaseSteps "Provides health status" $ \step ->
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, _orAuthUser) -> do
          let http = runClient baseurl

          step "Status results in 200"
          healthResult <- http health
          healthResult `shouldSatisfy` isRight
          healthResult `shouldBe` (Right HealthResponse)



  pure $ testGroup "Org Registry HTTP Client tests"
        [ orgTests
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
