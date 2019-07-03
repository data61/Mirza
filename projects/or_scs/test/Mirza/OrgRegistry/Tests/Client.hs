{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Mirza.OrgRegistry.Tests.Client where

import           Control.Concurrent               (threadDelay)
import           Control.Exception                (bracket)

import           Mirza.Common.Tests.ServantUtils
import           Mirza.Common.Utils

import qualified Servant.API.ContentTypes
import           Servant.Client

import           Network.URI                      (URI (..), URIAuth (..),
                                                   nullURI)

import           Data.ByteString.Lazy             (ByteString)

import           System.Directory                 (listDirectory)
import           System.FilePath                  ((</>))

import           Control.Monad                    (forM_)
import           Data.Either                      (isLeft, isRight)
import           Data.Either.Utils                (fromRight)
import           Data.Function                    ((&))
import           Data.List                        (isSuffixOf)
import           Data.Maybe                       (fromJust, isJust, isNothing)
import           Data.Time.Clock                  (addUTCTime, diffUTCTime,
                                                   getCurrentTime)
import           Data.UUID                        (nil)

import qualified Network.HTTP.Types.Status        as NS

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.GS1.EPC                     (GS1CompanyPrefix (..),
                                                   LocationEPC (SGLN),
                                                   LocationReference (LocationReference))

import           Mirza.OrgRegistry.Client.Servant
import           Mirza.OrgRegistry.Types

import           Mirza.Common.Time
import           Mirza.Common.Utils               (readJWK)

import           Mirza.Common.Tests.InitClient
import           Mirza.Common.Tests.Utils
import           Mirza.OrgRegistry.Tests.Utils


import           Control.Lens
import           Crypto.JWT



-- === OR Servant Client tests
clientSpec :: IO TestTree
clientSpec = do
  let orgTests = testCaseSteps "Can create orgs" $ \step ->
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid,baseurl,tokenData) -> do
          let http = runClient baseurl
              token = testToken tokenData
              org1Prefix   = GS1CompanyPrefix "2000001"
              org1Name     = "orgTests_org1Name"
              org1         = PartialNewOrg org1Name (mockURI org1Name)
              org1Response = partialNewOrgToOrgResponse org1Prefix org1
              org2Prefix   = GS1CompanyPrefix "2000002"
              org2Name     = "orgTests_org2Name"
              org2         =  PartialNewOrg org2Name (mockURI org2Name)
              org2Response = partialNewOrgToOrgResponse org2Prefix org2
              org3Prefix   = GS1CompanyPrefix "3000003"
              org3Name     = "A strange name"
              org3         =  PartialNewOrg org3Name (mockURI org3Name)
              org3Response = partialNewOrgToOrgResponse org3Prefix org3
              -- emptyPrefixOrg = NewOrg (GS1CompanyPrefix "") "EmptyOrg"
              -- stringPrefix1Org = NewOrg (GS1CompanyPrefix "string") "EmptyOrg"
              nullURIPrefix   = GS1CompanyPrefix  "3000004"
              invlaidURLPrefix   = GS1CompanyPrefix  "3000005"

          step "Can create a new org"
          addOrg1Result <- http (addOrg token org1Prefix org1)
          addOrg1Result `shouldSatisfy` isRight
          addOrg1Result `shouldBe` (Right Servant.API.ContentTypes.NoContent)

          step "That the user who added the org is associated with the org"
          orgSearchResult <- http (getOrgInfo token)
          orgSearchResult `shouldSatisfy` isRight
          orgSearchResult `shouldBe` Right [org1Response]

          step "That the added org was added and can be listed."
          http (searchOrgs Nothing Nothing Nothing) >>=
            either (const $ expectationFailure "Error listing orgs")
                   (`shouldContain` [org1Response])

          step "Can't add org with the same GS1CompanyPrefix"
          duplicatePrefixResult <- http (addOrg token org1Prefix org2)
          duplicatePrefixResult `shouldSatisfy` isLeft
          duplicatePrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          duplicatePrefixResult `shouldSatisfy` (checkFailureMessage "GS1 company prefix already exists.")

          step "Can add a second org"
          addOrg2Result <- http (addOrg token org2Prefix org2)
          addOrg2Result `shouldSatisfy` isRight
          addOrg2Result `shouldBe` (Right Servant.API.ContentTypes.NoContent)

          step "List orgs returns all of the orgs"
          http (searchOrgs Nothing Nothing Nothing) >>=
              either (const $ expectationFailure "Error listing orgs")
                    (`shouldContain` [ org1Response
                                     , org2Response])

          step "Can add a third org"
          addOrg3Result <- http (addOrg token org3Prefix org3)
          addOrg3Result `shouldSatisfy` isRight
          addOrg3Result `shouldBe` (Right Servant.API.ContentTypes.NoContent)

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
          -- emptyPrefixResult <- http (addOrg token emptyPrefixOrg)
          -- emptyPrefixResult `shouldSatisfy` isLeft
          -- emptyPrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- emptyPrefixResult `shouldSatisfy` (checkFailureMessage "TODO")

          -- TODO: Include me (github #205):
          -- This should possibly be changed to something that is within the
          -- type specification but is logically incorrect if the type
          -- constraint is improved.
          -- step "That the GS1CompanyPrefix can't be a string."
          -- stringPrefixResult <- http (addOrg token stringPrefix1Org)
          -- stringPrefixResult `shouldSatisfy` isLeft
          -- stringPrefixResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          -- stringPrefixResult `shouldSatisfy` (checkFailureMessage "TODO")

          -- TODO: That users who are not associated with an org can't add users to the org.
          -- TODO: Can associate multiple users with an org.
          -- TODO: That users who haven't been "registered" added can't be added to an org.
          -- TODO: That users who are not the initial user but who have been associated with the org can add more users.

          step "Can't add org with a null URL"
          nullURLResult <- http (addOrg token nullURIPrefix org1{partialNewOrgUrl = nullURI})
          nullURLResult `shouldSatisfy` isLeft
          nullURLResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          nullURLResult `shouldSatisfy` (checkFailureMessage "Error in $.url: not a URI")

          step "Can't add org with an invalid URL"
          invalidURLResult <- http (addOrg token invlaidURLPrefix org1{partialNewOrgUrl = URI "" (Just $ URIAuth "" "invalid" "") "" "" ""})
          invalidURLResult `shouldSatisfy` isLeft
          invalidURLResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          invalidURLResult `shouldSatisfy` (checkFailureMessage "Error in $.url: not a URI")


-- TODO: Move these to the top of the file now that the businesses depend on users rather then the other way round.
  let userTests = testCaseSteps "Can create users" $ \step ->
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid,baseurl,tokenData) -> do
          let http = runClient baseurl
              token = testToken tokenData

          step "Can add a new user"
          initalUserAddResult <- http (addUser token)
          initalUserAddResult `shouldSatisfy` isRight
          -- Note: We effectively implicitly test that the value returned is
          --       sensible later when we test that a user with this ID occurs
          --       in a keys query response, here we can only test that we think
          --       that we succeeded and have no other way of verifying the ID
          --       is otherwise correct constraining our selves to just user
          --       related API functions.

          step "That the created user can login and initially not assigned to any companies"
          orgSearchResult <- http (getOrgInfo token)
          orgSearchResult `shouldSatisfy` isRight
          orgSearchResult `shouldBe` Right []

          step "Can re-add a user with the same oAuth sub"
          duplicateEmailResult <- http (addUser token)
          duplicateEmailResult `shouldSatisfy` isRight

          step "Can add a second user"
          let secondUserClaims = (testTokenDefaultClaims tokenData)
                                 & claimSub .~ Just (review string "userTests_OAuthSub_SecondUser")
          secondUserToken <- (testSignTokenClaims tokenData) secondUserClaims
          secondUserAddResult <- http (addUser secondUserToken)
          secondUserAddResult `shouldSatisfy` isRight

          step "Can't create a user with an empty oAuth Sub."
          let emptySubClaims = (testTokenDefaultClaims tokenData)
                                 & claimSub .~ Just (review string "")
          emptySubToken <- (testSignTokenClaims tokenData) emptySubClaims
          emptySubAddResult <- http (addUser emptySubToken)
          emptySubAddResult `shouldSatisfy` isLeft
          emptySubAddResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          emptySubAddResult `shouldSatisfy` (checkFailureMessage "Invalid OAuth Sub.")

          -- TODO: Valid Token checks.
          -- TODO: Check when Auth AUD doesn't match.
          -- TODO: Check when Auth AUD is empty.
          -- TODO: Check when Token is expired.
          -- TODO: Check when Token isn't yets valid.
          -- TODO: Check that if the signature is invalid that the token is rejected.


  let keyTests = testCaseSteps "That keys work as expected" $ \step ->
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, tokenData) -> do
          let http = runClient baseurl
              org1Prefix = (GS1CompanyPrefix "4000001")
              org1Name = "keyTests_orgName1"
              org1 = PartialNewOrg org1Name (mockURI org1Name)
              org2Prefix = (GS1CompanyPrefix "4000002")
              org2Name = "keyTests_orgName2"
              org2 = PartialNewOrg org2Name (mockURI org2Name)

          -- Org1User1
          let userB1U1ClaimSub = "keysTests_OAuthSub_B1U1"
          let userB1U1 = (testTokenDefaultClaims tokenData)
                         & claimSub .~ Just (review string userB1U1ClaimSub)
          userB1U1Token <- (testSignTokenClaims tokenData) userB1U1
          -- Org1User2
          let userB1U2ClaimSub = "keysTests_OAuthSub_B1U2"
          let userB1U2 = (testTokenDefaultClaims tokenData)
                         & claimSub .~ Just (review string userB1U2ClaimSub)
          userB1U2Token <- (testSignTokenClaims tokenData) userB1U2
          -- Org2User1
          let userB2U1ClaimSub = "keysTests_OAuthSub_B2U1"
          let userB2U1 = (testTokenDefaultClaims tokenData)
                         & claimSub .~ Just (review string userB2U1ClaimSub)
          userB2U1Token <- (testSignTokenClaims tokenData) userB2U1

          -- Create a org to use from further test cases (this is tested in
          --  the orgs tests so doesn't need to be explicitly tested here).
          _ <- http (addOrg userB1U1Token org1Prefix org1)
          _ <- http (addOrg userB2U1Token org2Prefix org2)

          -- Add a second user to first org use from further test cases (this is tested in
          -- the orgs tests so doesn't need to be explicitly tested here).
          _ <- http (addUser userB1U2Token)
          _ <- http (addUserToOrg userB1U1Token org1Prefix $ OAuthSub userB1U2ClaimSub)

          -- Add good RSA Public Key for using from the test cases.
          Just goodKey <- goodRsaPublicKey

          step "Can add a good key (no exipry time)"
          b1K1PreInsertionTime <- getCurrentTime
          b1K1StoredKeyIdResult <- http (addPublicKey userB1U1Token org1Prefix goodKey Nothing)
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
                key1InfoId
                key1InfoOrg
                key1InfoState
                key1InfoCreationTime
                key1InfoRevocationTime
                key1InfoExpirationTime
                key1InfoPEMString
                = fromRight b1K1InfoResponse
          key1InfoId             `shouldSatisfy` (== b1K1StoredKeyId)
          key1InfoOrg            `shouldSatisfy` (== org1Prefix)
          key1InfoState          `shouldSatisfy` (== InEffect)
          key1InfoRevocationTime `shouldSatisfy` isNothing
          key1InfoExpirationTime `shouldSatisfy` isNothing
          key1InfoPEMString      `shouldSatisfy` (== goodKey)
          getCreationTime key1InfoCreationTime
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

          -- TODO: That a user that is not associated with the org can't add keys for it.

          let expiryDelay = 3
          step $ "Can add a good key with exipry time (" ++ (show expiryDelay) ++ " seconds from now)"
          b1K2Expiry <- (Just . ExpirationTime) <$> ((addUTCTime (fromInteger expiryDelay)) <$> getCurrentTime)
          b1K2StoredKeyIdResult <- http (addPublicKey userB1U1Token org1Prefix goodKey b1K2Expiry)
          b1K2StoredKeyIdResult `shouldSatisfy` isRight

          let Right b1K2StoredKeyId = b1K2StoredKeyIdResult

          step "That the key info reflects the expiry time"
          b1K2InfoResponse <- http (getPublicKeyInfo b1K2StoredKeyId)
          b1K2InfoResponse `shouldSatisfy` isRight
          let KeyInfoResponse
                key2InfoId
                key2InfoOrg
                key2InfoState
                _key2InfoCreationTime
                key2InfoRevocationTime
                key2InfoExpirationTime
                key2InfoPEMString
                = fromRight b1K2InfoResponse
          key2InfoId             `shouldSatisfy` (== b1K2StoredKeyId)
          key2InfoOrg            `shouldSatisfy` (== org1Prefix)
          key2InfoState          `shouldSatisfy` (== InEffect)
          key2InfoRevocationTime `shouldSatisfy` isNothing
          key2InfoPEMString      `shouldSatisfy` (== goodKey)
          getExpirationTime (fromJust key2InfoExpirationTime)
            `shouldSatisfy` within1Second ((getExpirationTime . fromJust) b1K2Expiry)

          step "That the key info status updates after the expiry time has been reached"
          threadDelay $ fromIntegral $ secondsToMicroseconds expiryDelay
          b1K2InfoDelayedResponse <- http (getPublicKeyInfo b1K2StoredKeyId)
          b1K2InfoDelayedResponse `shouldSatisfy` isRight
          b1K2InfoDelayedResponse `shouldSatisfy` checkField keyInfoState (== Expired)

          step "Test that it is not possible to revoke a key that has already expired."
          b1K2RevokedResponse <- http (revokePublicKey userB1U1Token b1K2StoredKeyId)
          b1K2RevokedResponse `shouldSatisfy` isLeft
          b1K2RevokedResponse `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          b1K2RevokedResponse `shouldSatisfy` (checkFailureMessage "Public key already expired.")

          step "That it is not possible to add a key that is already expired"
          b1ExpiredKeyExpiry <- (Just . ExpirationTime) <$> ((addUTCTime (fromInteger (-1))) <$> getCurrentTime)
          b1ExpiredKeyExpiryResult <- http (addPublicKey userB1U1Token org1Prefix goodKey b1ExpiredKeyExpiry)
          b1ExpiredKeyExpiryResult `shouldSatisfy` isLeft
          b1ExpiredKeyExpiryResult `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          b1ExpiredKeyExpiryResult `shouldSatisfy` (checkFailureMessage "Can't add a key that has already expired.")

          step "That it's possible to revoke a key"
          b1K3StoredKeyIdResult <- http (addPublicKey userB1U1Token org1Prefix goodKey Nothing)
          b1K3StoredKeyIdResult `shouldSatisfy` isRight
          let b1K3StoredKeyId = fromRight b1K3StoredKeyIdResult
          b1K3PreRevoke <- getCurrentTime
          b1K3RevokedResponse <- http (revokePublicKey userB1U1Token b1K3StoredKeyId)
          b1K3PostRevoke <- getCurrentTime
          b1K3RevokedResponse `shouldSatisfy` isRight
          b1K3RevokedResponse `shouldSatisfy` checkField getRevocationTime (betweenInclusive b1K3PreRevoke b1K3PostRevoke)

          step "That the key info correctly shows the revokation status time and revoking user"
          let extractRevocationTime = getRevocationTime . fst . fromJust
          b1K3InfoResponse <- http (getPublicKeyInfo b1K3StoredKeyId)
          b1K3InfoResponse `shouldSatisfy` isRight
          b1K3InfoResponse `shouldSatisfy` checkField keyInfoRevocation isJust
          b1K3InfoResponse `shouldSatisfy` checkField keyInfoRevocation ((betweenInclusive b1K3PreRevoke b1K3PostRevoke) . extractRevocationTime)
          b1K3InfoResponse `shouldSatisfy` checkField keyInfoRevocation ((== OAuthSub userB1U1ClaimSub) . snd . fromJust)
          -- We check that the time through this responce ~matches the time that was given when we revoked the key.
          let b1K3RevokedResponseTime = (getRevocationTime . fromRight) b1K3RevokedResponse
          b1K3InfoResponse `shouldSatisfy` checkField keyInfoRevocation ((within1Second b1K3RevokedResponseTime) . extractRevocationTime)

          step "That the key status updates after the key is revoked"
          b1K3RevokedInfoResponse <- http (getPublicKeyInfo b1K3StoredKeyId)
          b1K3RevokedInfoResponse `shouldSatisfy` isRight
          b1K3RevokedInfoResponse `shouldSatisfy` checkField keyInfoState (== Revoked)

          step "That revoking an already revoked key generates an error"
          b1K3RevokedAgainResponse <- http (revokePublicKey userB1U1Token b1K3StoredKeyId)
          b1K3RevokedAgainResponse `shouldSatisfy` isLeft
          b1K3RevokedAgainResponse `shouldSatisfy` (checkFailureStatus NS.badRequest400)
          b1K3RevokedAgainResponse `shouldSatisfy` (checkFailureMessage "Public key already revoked.")

          step "That another user from the same org can also revoke the key"
          b1K4StoredKeyIdResult <- http (addPublicKey userB1U1Token org1Prefix goodKey Nothing)
          b1K4StoredKeyIdResult `shouldSatisfy` isRight
          let b1K4StoredKeyId = fromRight b1K4StoredKeyIdResult
          b1K4RevokedResponse <- http (revokePublicKey userB1U2Token b1K4StoredKeyId)
          b1K4RevokedResponse `shouldSatisfy` isRight
          b1K4RevokedInfoResponse <- http (getPublicKeyInfo b1K4StoredKeyId)
          b1K4RevokedInfoResponse `shouldSatisfy` isRight
          b1K4RevokedInfoResponse `shouldSatisfy` (checkField keyInfoState (== Revoked))
          b1K4RevokedInfoResponse `shouldSatisfy` (checkField keyInfoRevocation ((== OAuthSub userB1U2ClaimSub) . snd . fromJust))
          -- Test integrity check to make sure that the user is not the original user.
          b1K4RevokedInfoResponse `shouldSatisfy` (checkField keyInfoRevocation ((/= OAuthSub userB1U1ClaimSub) . snd . fromJust))

          step "That a user from the another org can't also revoke the key"
          b1K5StoredKeyIdResult <- http (addPublicKey  userB1U1Token org1Prefix goodKey Nothing)
          b1K5StoredKeyIdResult `shouldSatisfy` isRight
          let Right b1K5StoredKeyId = b1K5StoredKeyIdResult
          b1K5RevokedResponse <- http (revokePublicKey userB2U1Token b1K5StoredKeyId)
          b1K5RevokedResponse `shouldSatisfy` isLeft
          b1K5RevokedResponse `shouldSatisfy` (checkFailureStatus NS.forbidden403)
          b1K5RevokedResponse `shouldSatisfy` (checkFailureMessage "A user can only act on behalf of the org they are associated with.")
          b1K5RevokedInfoResponse <- http (getPublicKeyInfo b1K5StoredKeyId)
          b1K5RevokedInfoResponse `shouldSatisfy` isRight
          b1K5RevokedInfoResponse `shouldSatisfy` checkField keyInfoState (== InEffect)

          step "That revokePublicKey for an invalid keyId fails gracefully"
          revokeInvalidKeyIdResponse <- http (revokePublicKey userB1U1Token (ORKeyId nil))
          revokeInvalidKeyIdResponse `shouldSatisfy` isLeft
          revokeInvalidKeyIdResponse `shouldSatisfy` (checkFailureStatus NS.notFound404)
          revokeInvalidKeyIdResponse `shouldSatisfy` (checkFailureMessage "Public key with the given id not found.")

          step "Test where the key has an expiry time (which hasn't expired) and is revoked reports the correct status."
          b1K6ExpiryUTC <- (addUTCTime (fromInteger expiryDelay)) <$> getCurrentTime
          let b1K6Expiry = Just . ExpirationTime $ b1K6ExpiryUTC
          b1K6StoreKeyIdResult <- http (addPublicKey userB1U1Token org1Prefix goodKey b1K6Expiry)
          b1K6StoreKeyIdResult `shouldSatisfy` isRight
          let Right b1K6KeyId = b1K6StoreKeyIdResult
          b1K6RevokeKeyResult <- http (revokePublicKey userB1U1Token b1K6KeyId)
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
                  http (addPublicKey userB1U1Token org1Prefix key Nothing)
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
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, tokenData) -> do
          let http = runClient baseurl
              org1Prefix = (GS1CompanyPrefix "5000001")
              org1Name = "locationTests_orgName1"
              org1 = PartialNewOrg org1Name (mockURI org1Name)
              org2Prefix = (GS1CompanyPrefix "5000002")
              org2Name = "locationTests_orgName2"
              org2 = PartialNewOrg org2Name (mockURI org2Name)
              org3Prefix = (GS1CompanyPrefix "5000003")
              org3Name = "locationTests_orgName3"
              org3 = PartialNewOrg org3Name (mockURI org3Name)

          -- Org1User1
          let userB1U1ClaimSub = "keysTests_OAuthSub_B1U1"
          let userB1U1 = (testTokenDefaultClaims tokenData)
                         & claimSub .~ Just (review string userB1U1ClaimSub)
          userB1U1Token <- (testSignTokenClaims tokenData) userB1U1
          -- Org2User1
          let userB2U1ClaimSub = "keysTests_OAuthSub_B2U1"
          let userB2U1 = (testTokenDefaultClaims tokenData)
                         & claimSub .~ Just (review string userB2U1ClaimSub)
          userB2U1Token <- (testSignTokenClaims tokenData) userB2U1
          -- Org3User1
          let userB3U1ClaimSub = "keysTests_OAuthSub_B3U1"
          let userB3U1 = (testTokenDefaultClaims tokenData)
                         & claimSub .~ Just (review string userB3U1ClaimSub)
          userB3U1Token <- (testSignTokenClaims tokenData) userB3U1

          -- Create a org to use from further test cases (this is tested in
          --  the orgs tests so doesn't need to be explicitly tested here).
          _ <- http (addOrg userB1U1Token org1Prefix org1)
          _ <- http (addOrg userB2U1Token org2Prefix org2)
          _ <- http (addOrg userB3U1Token org3Prefix org3)


          step "Can add a location"
          let location1 = NewLocation (SGLN org1Prefix (LocationReference "00011") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation1Result <- http (addLocation userB1U1Token location1)
          addLocation1Result `shouldSatisfy` isRight


          -- step "TODO: Can't add a location for a company that doesn't exist."
          -- let nonExistantCompanyPrefix = (GS1CompanyPrefix "5999999")
          --     location2 = NewLocation (SGLN nonExistantCompanyPrefix (LocationReference "00013") Nothing)
          --                             (Just (Latitude (-25.344490), Longitude 131.035431))
          --                             (Just "42 Wallby Way, Sydney")
          -- addLocation2Result1 <- http (addLocation token location2)
          -- addLocation2Result1 `shouldSatisfy` isLeft
          -- addLocation2Result2 <- http (addLocation (newUserToBasicAuthData userB1U1) location2)
          -- addLocation2Result2 `shouldSatisfy` isLeft


          step "That a user can only insert a location from their busniess."
          let location3 = NewLocation (SGLN org2Prefix (LocationReference "00017") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation3Result <- http (addLocation userB1U1Token location3)
          addLocation3Result `shouldSatisfy` isLeft


          step "Can add a second location with a different company."
          addLocation4Result <- http (addLocation userB2U1Token location3)
          addLocation4Result `shouldSatisfy` isRight


          step "Can add a second location with the same company."
          let location5 = NewLocation (SGLN org1Prefix (LocationReference "00019") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation5Result <- http (addLocation userB1U1Token location5)
          addLocation5Result `shouldSatisfy` isRight


          step "Can't add a location with a duplicate LocationReference for the same org."
          let location6 = NewLocation (SGLN org1Prefix (LocationReference "00019") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation6Result <- http (addLocation userB1U1Token location6)
          addLocation6Result `shouldSatisfy` isLeft


          step "Can add a location with a duplicate LocationReference for different orgs."
          let location7 = NewLocation (SGLN org3Prefix (LocationReference "00019") Nothing)
                                      (Just (Latitude (-25.344490), Longitude 131.035431))
                                      (Just "42 Wallby Way, Sydney")
          addLocation7Result <- http (addLocation userB3U1Token location7)
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
        bracket runORApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, _tokenData) -> do
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


partialNewOrgToOrgResponse :: GS1CompanyPrefix -> PartialNewOrg -> OrgResponse
partialNewOrgToOrgResponse gs1CompanyPrefix partialNewOrg = OrgResponse
  { orgResponseGS1CompanyPrefix = gs1CompanyPrefix
  , orgResponseName             = partialNewOrgName partialNewOrg
  , orgResponseUrl              = partialNewOrgUrl partialNewOrg
  }
