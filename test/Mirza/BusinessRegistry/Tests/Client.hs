{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Mirza.BusinessRegistry.Tests.Client where


import           Control.Concurrent                     (ThreadId, threadDelay)
import           Control.Exception                      (bracket)

import           Mirza.Common.Tests.ServantUtils

import           Servant.API.BasicAuth
import           Servant.Client

import           Control.Monad                          (forM_)
import           Data.Either                            (isLeft, isRight)
import           Data.Either.Utils                      (fromRight)
import           Data.List                              (isSuffixOf)
import           Data.Maybe                             (fromJust, isNothing)
import           Data.Text                              (Text)
import           Data.Text.Encoding                     (encodeUtf8)
import           Data.Time.Clock                        (addUTCTime,
                                                        diffUTCTime,
                                                        getCurrentTime)
import           Data.UUID                              (nil)

import           System.Directory                       (listDirectory)
import           System.FilePath                        ((</>))

import           Database.Beam.Query                    (delete, runDelete,
                                                        val_)

import           Katip                                  (Severity (DebugS))

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.GS1.EPC                           (GS1CompanyPrefix (..))

import           Mirza.BusinessRegistry.Client.Servant
import           Mirza.BusinessRegistry.Database.Schema
import qualified Mirza.BusinessRegistry.Handlers.Business as BRHB (addBusiness)
import qualified Mirza.BusinessRegistry.Handlers.Users    as BRHU (addUserQuery)
import           Mirza.BusinessRegistry.Main              (GlobalOptions (..),
                                                          RunServerOptions (..),
                                                          initApplication,
                                                          initBRContext)
import           Mirza.BusinessRegistry.Types

import           Mirza.Common.Time
import           Mirza.Common.Utils

import           Mirza.BusinessRegistry.Tests.Settings  (testDbConnStr)
import           Mirza.BusinessRegistry.Tests.Utils
import           Mirza.Common.Tests.Utils

-- === BR Servant Client tests

clientSpec :: IO TestTree
clientSpec = do
  let businessTests = testCaseSteps "Can create businesses" $ \step ->
        bracket runApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid,baseurl,globalAuthData) -> do
          let http = runClient baseurl
              biz1Prefix = (GS1CompanyPrefix "2000001")
              biz1 = NewBusiness biz1Prefix "businessTests_biz1Name"
              biz1Response = newBusinessToBusinessResponse biz1
              biz2Prefix = (GS1CompanyPrefix "2000002")
              biz2 =  NewBusiness biz2Prefix "businessTests_biz2Name"
              biz2Response = newBusinessToBusinessResponse biz2
              -- emptyPrefixBiz = NewBusiness (GS1CompanyPrefix "") "EmptyBusiness"
              -- stringPrefix1Biz = NewBusiness (GS1CompanyPrefix "string") "EmptyBusiness"

          step "Can create a new business"
          addBiz1Result <- http (addBusiness globalAuthData biz1)
          addBiz1Result `shouldSatisfy` isRight
          addBiz1Result `shouldBe` (Right biz1Prefix)

          step "That the added business was added and can be listed."
          http listBusinesses >>=
            either (const $ expectationFailure "Error listing businesses")
                   (`shouldContain` [biz1Response])

          step "Can't add business with the same GS1CompanyPrefix"
          http (addBusiness globalAuthData biz1{newBusinessName = "businessTests_anotherName"})
            `shouldSatisfyIO` isLeft
          -- Should also check that the error type is correct / meaningful.

          step "Can add a second business"
          addBiz2Result <- http (addBusiness globalAuthData biz2)
          addBiz2Result `shouldSatisfy` isRight
          addBiz2Result `shouldBe` (Right biz2Prefix)

          step "List businesses returns all of the businesses"
          http listBusinesses >>=
              either (const $ expectationFailure "Error listing businesses")
                    (`shouldContain` [ biz1Response
                                        , biz2Response])

          -- TODO: Include me (github #205):
          -- step "That the GS1CompanyPrefix can't be empty (\"\")."
          -- http (addBusiness globalAuthData emptyPrefixBiz)
          --   `shouldSatisfyIO` isLeft

          -- TODO: Include me (github #205):
          -- step "That the GS1CompanyPrefix can't be a string."
          -- http (addBusiness globalAuthData stringPrefix1Biz)
          --    `shouldSatisfyIO` isLeft


  let userTests = testCaseSteps "Can create users" $ \step ->
        bracket runApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid,baseurl,globalAuthData) -> do
          password <- randomPassword

          let http = runClient baseurl
              companyPrefix = (GS1CompanyPrefix "3000001")
              business = NewBusiness companyPrefix "userTests_businessName"

          let user1 = NewUser (EmailAddress "userTests_email1@example.com")
                              password
                              companyPrefix
                              "userTests First Name 1"
                              "userTests Last Name 1"
                              "userTests Phone Number 1"
              user2 = NewUser (EmailAddress "userTests_email2@example.com")
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
              userNonRegisteredBiz = NewUser (EmailAddress "userTests_unregisteredBusiness@example.com")
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
          _ <- http (addBusiness globalAuthData business)

          -- Add good RSA Public Key for using from the test cases.
          goodKey <- goodRsaPublicKey

          -- We delibrately test the "good user" that we will later add so that
          -- we know that we are failing because they aren't in the DB rather
          -- then because they are somehow otherwise invalid.
          step "That a user that doesn't exist can't login"
          http (addPublicKey (newUserToBasicAuthData user1) goodKey Nothing)
            `shouldSatisfyIO` isLeft

          step "Can create a new user"
          http (addUser globalAuthData user1)
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
          http (addPublicKey (newUserToBasicAuthData user1){basicAuthPassword = "invalid password"} goodKey Nothing)
            `shouldSatisfyIO` isLeft

          step "That the an empty password doesn't allow the user to login"
          http (addPublicKey (newUserToBasicAuthData user1){basicAuthPassword = ""} goodKey Nothing)
            `shouldSatisfyIO` isLeft

          step "Can't create a new user with a GS1CompanyPrefix that isn't registered"
          http (addUser globalAuthData userNonRegisteredBiz)
            `shouldSatisfyIO` isLeft

          step "Can't create a new user with the same email address"
          http (addUser globalAuthData userSameEmail)
            `shouldSatisfyIO` isLeft

          step "Can create a second user"
          http (addUser globalAuthData user2)
            `shouldSatisfyIO` isRight

          -- TODO: Include me (github #205):
          -- step "Can't create a user with an empty email."
          -- http (addUser globalAuthData userEmptyEmail)
          --   `shouldSatisfyIO` isLeft

          -- TODO: Include me (github #205):
          -- step "Can't create a user with an empty password."
          -- http (addUser globalAuthData userEmptyPassword)
          --   `shouldSatisfyIO` isLeft


  let keyTests = testCaseSteps "That keys work as expected" $ \step ->
        bracket runApp (\(a,b,_) -> endWaiApp (a,b)) $ \(_tid, baseurl, globalAuthData) -> do
          password <- randomPassword
          let http = runClient baseurl
              biz1Prefix = (GS1CompanyPrefix "4000001")
              biz1 = NewBusiness biz1Prefix "userTests_businessName1"
              biz2Prefix = (GS1CompanyPrefix "4000002")
              biz2 = NewBusiness biz2Prefix "userTests_businessName2"

          -- Business1User1
          let userB1U1 = NewUser (EmailAddress "keysTests_email1@example.com")
                                 password
                                 biz1Prefix
                                 "keysTests First Name 1"
                                 "keysTests Last Name 1"
                                 "keysTests Phone Number 1"
          -- Business1User2
          let userB1U2 = NewUser (EmailAddress "keysTests_email2@example.com")
                                 password
                                 biz1Prefix
                                 "keysTests First Name 2"
                                 "keysTests Last Name 2"
                                 "keysTests Phone Number 2"
          -- Business2User1
          let userB2U1 = NewUser (EmailAddress "keysTests_email3@example.com")
                                 password
                                 biz2Prefix
                                 "keysTests First Name 3"
                                 "keysTests Last Name 3"
                                 "keysTests Phone Number 3"

          -- Create a business to use from further test cases (this is tested in
          --  the businesses tests so doesn't need to be explicitly tested here).
          _ <- http (addBusiness globalAuthData biz1)
          _ <- http (addBusiness globalAuthData biz2)

          -- Create a business to use from further test cases (this is tested in
          -- the businesses tests so doesn't need to be explicitly tested here).
          userB1U1Response <- http (addUser globalAuthData userB1U1)
          _                <- http (addUser globalAuthData userB1U2)
          _                <- http (addUser globalAuthData userB2U1)

          -- Add good RSA Public Key for using from the test cases.
          goodKey <- goodRsaPublicKey

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

          step "That getPublicKeyInfo fails gracefully searching for a non existant key"
          b1InvalidKeyInfoResponse <- http (getPublicKeyInfo (BRKeyId nil))
          b1InvalidKeyInfoResponse `shouldSatisfy` isLeft

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

          -- TODO Include this test (github #217):
          -- step "Test that it is not possible to revoke a key that has already expired."
          -- b1K2RevokedResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K2StoredKeyId)
          -- b1K2RevokedResponse `shouldSatisfy` isLeft

          -- TODO: Include this test (github #205):
          -- step $ "That it is not possible to add a key that is already expired"
          -- b1ExpiredKeyExpiry <- (Just . ExpirationTime) <$> ((addUTCTime (fromInteger (-1))) <$> getCurrentTime)
          -- b1ExpiredKeyExpiryResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey b1ExpiredKeyExpiry)
          -- b1ExpiredKeyExpiryResult `shouldSatisfy` isLeft

          step "That it's possible to revoke a key"
          b1K3StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          b1K3StoredKeyIdResult `shouldSatisfy` isRight
          let b1K3StoredKeyId = fromRight b1K3StoredKeyIdResult
          b1K3PreRevoke <- getCurrentTime
          b1K3RevokedResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K3StoredKeyId)
          b1K3PostRevoke <- getCurrentTime
          b1K3RevokedResponse `shouldSatisfy` isRight
          b1K3RevokedResponse `shouldSatisfy` checkField getRevocationTime (betweenInclusive b1K3PreRevoke b1K3PostRevoke)

          step "That the key status updates after the key is revoked"
          b1K3RevokedInfoResponse <- http (getPublicKeyInfo b1K3StoredKeyId)
          b1K3RevokedInfoResponse `shouldSatisfy` isRight
          b1K3RevokedInfoResponse `shouldSatisfy` checkField keyInfoState (== Revoked)

          step "That revoking an already revoked key generates an error"
          b1K3RevokedAgainResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K3StoredKeyId)
          b1K3RevokedAgainResponse `shouldSatisfy` isLeft

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

          step "That a user from the another business can't also revoke the key"
          b1K5StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          b1K5StoredKeyIdResult `shouldSatisfy` isRight
          let Right b1K5StoredKeyId = b1K5StoredKeyIdResult
          b1K5RevokedResponse <- http (revokePublicKey (newUserToBasicAuthData userB2U1) b1K5StoredKeyId)
          b1K5RevokedResponse `shouldSatisfy` isLeft
          b1K5RevokedInfoResponse <- http (getPublicKeyInfo b1K5StoredKeyId)
          b1K5RevokedInfoResponse `shouldSatisfy` isRight
          b1K5RevokedInfoResponse `shouldSatisfy` checkField keyInfoState (== InEffect)

          step "That revokePublicKey for an invalid keyId fails gracefully"
          revokeInvalidKeyIdResponse <- http (revokePublicKey (newUserToBasicAuthData userB1U1) (BRKeyId nil))
          revokeInvalidKeyIdResponse `shouldSatisfy` isLeft

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
          let testDirectory keyDirectory predicate = do
                let directory = "test" </> "Mirza" </> "Common" </> "TestData" </> "testKeys" </> keyDirectory
                files <- filter (".pub" `isSuffixOf`) <$> listDirectory directory
                let fullyQualifiedFiles = (directory </>) <$> files
                keys <- traverse readRsaPubKey fullyQualifiedFiles
                forM_ (zip files keys) $ \(keyName,key) -> do
                  step $ "Testing " ++ keyDirectory ++ " key: " ++ keyName
                  http (addPublicKey (newUserToBasicAuthData userB1U1) key Nothing)
                    `shouldSatisfyIO` predicate

          step "Can add all of the good keys"
          testDirectory "goodKeys" isRight

          -- TODO: Include this test. Some of the invalid keys work. (github #212)
          -- step "Can't add any of the bad keys"
          -- testDirectory "badKeys" isLeft


  pure $ testGroup "Business Registry HTTP Client tests"
        [ businessTests
        , userTests
        , keyTests
        ]


-- *****************************************************************************
-- Test Utility Functions
-- *****************************************************************************

go :: GlobalOptions
go = GlobalOptions testDbConnStr 14 8 1 DebugS Dev

runApp :: IO (ThreadId, BaseUrl, BasicAuthData)
runApp = do
  ctx <- initBRContext go
  let BusinessRegistryDB usersTable businessesTable keysTable
        = businessRegistryDB

  flushDbResult <- runAppM @_ @BusinessRegistryError ctx $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable keysTable
      deleteTable usersTable
      deleteTable businessesTable
  flushDbResult `shouldSatisfy` isRight

  -- This construct somewhat destroys the integrity of these test since it is
  -- necessary to assume that these functions work correctly in order for the
  -- test cases to complete.
  globalAuthData <- bootstrapAuthData ctx

  (tid,brul) <- startWaiApp =<< initApplication go (RunServerOptions 8000) ctx
  pure (tid,brul,globalAuthData)



-- *****************************************************************************
-- Test Utility Functions
-- *****************************************************************************

newBusinessToBusinessResponse :: NewBusiness -> BusinessResponse
newBusinessToBusinessResponse =
  BusinessResponse <$> newBusinessGS1CompanyPrefix <*> newBusinessName


newUserToBasicAuthData :: NewUser -> BasicAuthData
newUserToBasicAuthData =
  BasicAuthData
  <$> encodeUtf8 . getEmailAddress . newUserEmailAddress
  <*> encodeUtf8 . newUserPassword


-- Test helper function that enables a predicate to be run on the result of a
-- test call.
checkField :: (b -> a) -> (a -> Bool) -> Either c b -> Bool
checkField accessor predicate = either (const False) (predicate . accessor)


secondsToMicroseconds :: (Num a) => a -> a
secondsToMicroseconds = (* 1000000)


bootstrapAuthData :: (HasEnvType w, HasConnPool w, HasKatipContext w,
                      HasKatipLogEnv w, HasScryptParams w)
                     => w -> IO BasicAuthData
bootstrapAuthData ctx = do
  let email = "initialUser@example.com"
  password <- randomPassword
  let prefix = GS1CompanyPrefix "1000000"
  let business = NewBusiness prefix "Business Name"
  insertBusinessResult  <- runAppM @_ @BusinessRegistryError ctx $ BRHB.addBusiness business
  insertBusinessResult `shouldSatisfy` isRight
  let user = NewUser  (EmailAddress email)
                      password
                      prefix
                      "Test User First Name"
                      "Test User Last Name"
                      "Test User Phone Number"
  insertUserResult <- runAppM @_ @BusinessRegistryError ctx $ runDb (BRHU.addUserQuery user)
  insertUserResult `shouldSatisfy` isRight

  return $ newUserToBasicAuthData user


-- We specifically prefix the password with "PlainTextPassword:" so that it
-- makes it more obvious if this password shows up anywhere in plain text by
-- mistake.
randomPassword :: IO Text
randomPassword = ("PlainTextPassword:" <>) <$> randomText
