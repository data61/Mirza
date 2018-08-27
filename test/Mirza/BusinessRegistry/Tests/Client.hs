{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Mirza.BusinessRegistry.Tests.Client where

import           Mirza.BusinessRegistry.Tests.Settings  (testDbConnStr)

import           Control.Concurrent                     (ThreadId, forkIO,
                                                         killThread,
                                                         threadDelay)
import           Control.Exception                      (bracket)
import           System.IO.Unsafe                       (unsafePerformIO)

import qualified Network.HTTP.Client                    as C
import           Network.Socket
import qualified Network.Wai                            as Wai
import           Network.Wai.Handler.Warp

import           Servant.API.BasicAuth
import           Servant.Client

import           Control.Monad                          (forM_, replicateM)
import           Data.Either                            (isLeft, isRight)
import           Data.List                              (isSuffixOf)
import           Data.Maybe                             (fromJust, isNothing)
import           Data.Text                              (Text, pack)
import           Data.Text.Encoding                     (encodeUtf8)
import qualified Data.Text.IO                           as TIO
import           Data.Time.Clock                        (UTCTime, addUTCTime,
                                                        diffUTCTime,
                                                        getCurrentTime)
import           Data.UUID                              (nil)
import           System.Directory                       (listDirectory)
import           System.FilePath                        ((</>))
import           System.IO                              (FilePath)
import           System.Random


import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Database.Beam.Query                    (delete, runDelete,
                                                        val_)

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

import           Data.GS1.EPC                           (GS1CompanyPrefix (..))


import           Katip                                  (Severity (DebugS))

-- Cribbed from https://github.com/haskell-servant/servant/blob/master/servant-client/test/Servant/ClientSpec.hs

-- === Servant Client tests

clientSpec :: IO TestTree
clientSpec = do
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
  _globalAuthData <- bootstrapAuthData ctx

  let businessTests = testCaseSteps "Can create businesses" $ \step ->
        bracket runApp endWaiApp $ \(_tid,baseurl) -> do
          let http = runClient baseurl
              primaryCompanyPrefix = (GS1CompanyPrefix "businessTests_primaryCompanyPrefix")
              primaryBusiness = NewBusiness primaryCompanyPrefix "businessTests_primaryBusinessName"
              primaryBusinessResponse = newBusinessToBusinessResponse primaryBusiness
              secondaryCompanyPrefix = (GS1CompanyPrefix "businessTests_secondaryCompanyPrefix")
              secondaryBusiness =  NewBusiness secondaryCompanyPrefix "businessTests_secondaryBusinessName"
              secondaryBusinessResponse = newBusinessToBusinessResponse secondaryBusiness
              -- emptyCompanyPrefixBusiness = NewBusiness (GS1CompanyPrefix "") "EmptyBusiness"

          step "Can create a new business"
          newPrimaryBusinessResult <- http (addBusiness primaryBusiness)
          newPrimaryBusinessResult `shouldSatisfy` isRight
          newPrimaryBusinessResult `shouldBe` (Right primaryCompanyPrefix)

          step "That the added business was added and can be listed."
          http listBusinesses >>=
            either (const $ expectationFailure "Error listing businesses")
                   (`shouldContain` [primaryBusinessResponse])

          step "Can't add business with the same GS1CompanyPrefix"
          http (addBusiness primaryBusiness{newBusinessName = "businessTests_anotherName"})
            `shouldSatisfyIO` isLeft
          -- Should also check that the error type is correct / meaningful.

          step "Can add a second business"
          newSecondaryBusinessResult <- http (addBusiness secondaryBusiness)
          newSecondaryBusinessResult `shouldSatisfy` isRight
          newSecondaryBusinessResult `shouldBe` (Right secondaryCompanyPrefix)

          step "List businesses returns all of the businesses"
          http listBusinesses >>=
              either (const $ expectationFailure "Error listing businesses")
                    (`shouldContain` [ primaryBusinessResponse
                                        , secondaryBusinessResponse])

          -- TODO: Include me (github #205):
          -- step "That the GS1CompanyPrefix can't be empty (\"\")."
          -- http (addBusiness emptyCompanyPrefixBusiness)
          --   `shouldSatisfyIO` isLeft


  let userTests = testCaseSteps "Can create users" $ \step ->
        bracket runApp endWaiApp $ \(_tid,baseurl) -> do
          let http = runClient baseurl
              companyPrefix = (GS1CompanyPrefix "userTests_companyPrefix")
              business = NewBusiness companyPrefix "userTests_businessName"

          let user1 = NewUser (EmailAddress "userTests_email1@example.com")
                              "password"
                              companyPrefix
                              "userTests First Name 1"
                              "userTests Last Name 1"
                              "userTests Phone Number 1"
              user2 = NewUser (EmailAddress "userTests_email2@example.com")
                              "password"
                              companyPrefix
                              "userTests First Name 2"
                              "userTests Last Name 2"
                              "userTests Phone Number 2"
              -- Same email address as user1 other fields different.
              userSameEmail = NewUser (newUserEmailAddress user1)
                                      "password"
                                      companyPrefix
                                      "userTests First Name Same Email"
                                      "userTests Last Name Same Email"
                                      "userTests Phone Number Same Email"
              userNonRegisteredBusiness = NewUser (EmailAddress "userTests_unregisteredBusiness@example.com")
                                                  "password"
                                                  (GS1CompanyPrefix "unregistered")
                                                  "userTests First Name Unregistered Business"
                                                  "userTests Last Name Unregistered Business"
                                                  "userTests Phone Number Unregistered Business"
              -- userEmptyEmail = NewUser (EmailAddress "")
              --                          "password"
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
          _ <- http (addBusiness business)

          -- Add good RSA Public Key for using from the test cases.
          goodKey <- goodRsaPublicKey

          -- We delibrately test the "good user" that we will later add so that
          -- we know that we are failing because they aren't in the DB rather
          -- then because they are somehow otherwise invalid.
          step "That a user that doesn't exist can't login"
          http (addPublicKey (newUserToBasicAuthData user1) goodKey Nothing)
            `shouldSatisfyIO` isLeft

          step "Can create a new user"
          http (addUser user1)
            `shouldSatisfyIO` isRight
          -- Note: We effectively implicitly test that the value returned is
          --       sensible later when we test that a user with this ID occurs
          --       in a keys query responce, here we can only test that we think
          --       that we succeeded and have no other way of verifying the ID
          --       is otherwise correct constraining our selves to just user
          --       related API functions.

          step "That the created user can login"
          http (addPublicKey (newUserToBasicAuthData user1) goodKey Nothing)
            `shouldSatisfyIO` isRight
          -- Note: We test the result of the function elsewhere, all we care
          --       about here is that the user can login.

          step "Can't create a new user with a GS1CompanyPrefix that isn't registered"
          http (addUser userNonRegisteredBusiness)
            `shouldSatisfyIO` isLeft

          step "Can't create a new user with the same email address"
          http (addUser userSameEmail)
            `shouldSatisfyIO` isLeft

          step "Can create a second user"
          http (addUser user2)
            `shouldSatisfyIO` isRight

          -- TODO: Include me (github #205):
          -- step "Can't create a user with an empty email."
          -- http (addUser userEmptyEmail)
          --   `shouldSatisfyIO` isLeft

          -- TODO: Include me (github #205):
          -- step "Can't create a user with an empty password."
          -- http (addUser userEmptyPassword)
          --   `shouldSatisfyIO` isLeft


  let keyTests = testCaseSteps "That keys work as expected" $ \step ->
        bracket runApp endWaiApp $ \(_tid, baseurl) -> do
          -- Note: These tests assumes that the time on the server and the
          -- client is exactly the same. This "should" be true for the tests
          -- since they should be running on the same machine and using the same
          -- time source. But this test may need to be update if this assumption
          --  no longer holds for some reason in the future.

          let http = runClient baseurl
              business1CompanyPrefix = (GS1CompanyPrefix "keyTests_companyPrefix1")
              business1 = NewBusiness business1CompanyPrefix "userTests_businessName1"
              business2CompanyPrefix = (GS1CompanyPrefix "keyTests_companyPrefix2")
              business2 = NewBusiness business2CompanyPrefix "userTests_businessName2"

          -- Business1User1
          let userB1U1 = NewUser (EmailAddress "keysTests_email1@example.com")
                                 "password"
                                 business1CompanyPrefix
                                 "keysTests First Name 1"
                                 "keysTests Last Name 1"
                                 "keysTests Phone Number 1"
          -- Business1User2
          let userB1U2 = NewUser (EmailAddress "keysTests_email2@example.com")
                                 "password"
                                 business1CompanyPrefix
                                 "keysTests First Name 2"
                                 "keysTests Last Name 2"
                                 "keysTests Phone Number 2"
          -- Business2User1
          let userB2U1 = NewUser (EmailAddress "keysTests_email3@example.com")
                                 "password"
                                 business2CompanyPrefix
                                 "keysTests First Name 3"
                                 "keysTests Last Name 3"
                                 "keysTests Phone Number 3"

          -- Create a business to use from further test cases (this is tested in
          --  the businesses tests so doesn't need to be explicitly tested here).
          _ <- http (addBusiness business1)
          _ <- http (addBusiness business2)

          -- Create a business to use from further test cases (this is tested in
          -- the businesses tests so doesn't need to be explicitly tested here).
          userB1U1Responce <- http (addUser userB1U1)
          _                <- http (addUser userB1U2)
          _                <- http (addUser userB2U1)

          -- Add good RSA Public Key for using from the test cases.
          goodKey <- goodRsaPublicKey

          step "Can add a good key (no exipry time)"
          b1K1ApproxInsertionTime <- getCurrentTime
          b1K1StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          b1K1StoredKeyIdResult `shouldSatisfy` isRight

          let b1K1StoredKeyId = right b1K1StoredKeyIdResult

          step "Can retrieve a stored key"
          b1K1Responce <- http (getPublicKey b1K1StoredKeyId)
          b1K1Responce `shouldSatisfy` isRight

          step "Can retrieve the key info for a stored key"
          b1K1InfoResponce <- http (getPublicKeyInfo b1K1StoredKeyId)
          b1K1InfoResponce `shouldSatisfy` isRight
          b1K1InfoResponce `shouldSatisfy` (checkRecord (b1K1StoredKeyId ==) keyInfoId)
          b1K1InfoResponce `shouldSatisfy` (checkRecord (right userB1U1Responce ==) keyInfoUserId)
          b1K1InfoResponce `shouldSatisfy` (checkRecord (InEffect ==) keyInfoState)
          b1K1InfoResponce `shouldSatisfy` (checkRecord (within1Second b1K1ApproxInsertionTime) (getCreationTime . keyInfoCreationTime))
          b1K1InfoResponce `shouldSatisfy` (checkRecord isNothing keyInfoRevocationTime)
          b1K1InfoResponce `shouldSatisfy` (checkRecord isNothing keyInfoExpirationTime)
          b1K1InfoResponce `shouldSatisfy` (checkRecord (goodKey ==) keyInfoPEMString)

          step "That getPublicKey fails gracefully searching for a non existant key"
          b1InvalidKeyResponce <- http (getPublicKey (BRKeyId nil))
          b1InvalidKeyResponce `shouldSatisfy` isLeft

          step "That getPublicKeyInfo fails gracefully searching for a non existant key"
          b1InvalidKeyInfoResponce <- http (getPublicKeyInfo (BRKeyId nil))
          b1InvalidKeyInfoResponce `shouldSatisfy` isLeft

          let expiryDelay = 3
          step $ "Can add a good key with exipry time (" ++ (show expiryDelay) ++ " seconds from now)"
          b1K2Expiry <- (Just . ExpirationTime) <$> ((addUTCTime (fromInteger expiryDelay)) <$> getCurrentTime)
          b1K2StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey b1K2Expiry)
          b1K2StoredKeyIdResult `shouldSatisfy` isRight

          let b1K2StoredKeyId = right b1K2StoredKeyIdResult

          step "That the key info reflects the expiry time"
          b1K2InfoResponce <- http (getPublicKeyInfo b1K2StoredKeyId)
          b1K2InfoResponce `shouldSatisfy` isRight
          b1K2InfoResponce `shouldSatisfy` (checkRecord (b1K2StoredKeyId ==) keyInfoId)
          b1K2InfoResponce `shouldSatisfy` (checkRecord (right userB1U1Responce ==) keyInfoUserId)
          b1K2InfoResponce `shouldSatisfy` (checkRecord (InEffect ==) keyInfoState)
          b1K2InfoResponce `shouldSatisfy` (checkRecord isNothing keyInfoRevocationTime)
          b1K2InfoResponce `shouldSatisfy` (checkRecord (within1Second ((getExpirationTime . fromJust) b1K2Expiry)) (getExpirationTime . fromJust . keyInfoExpirationTime))
          b1K2InfoResponce `shouldSatisfy` (checkRecord (goodKey ==) keyInfoPEMString)

          step "That the key info status updates after the expiry time has been reached"
          threadDelay $ fromIntegral $ millisecondsToSeconds expiryDelay
          b1K2InfoDelayedResponce <- http (getPublicKeyInfo b1K2StoredKeyId)
          b1K2InfoDelayedResponce `shouldSatisfy` isRight
          b1K2InfoDelayedResponce `shouldSatisfy` (checkRecord (Expired ==) keyInfoState)

          -- TODO Include this test (github #217):
          -- step "Test that it is not possible to revoke a key that has already expired."
          -- b1K2RevokedResponce <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K2StoredKeyId)
          -- b1K2RevokedResponce `shouldSatisfy` isLeft

          -- TODO: Include this test (github #205):
          -- step $ "That it is not possible to add a key that is already expired"
          -- b1ExpiredKeyExpiry <- (Just . ExpirationTime) <$> ((addUTCTime (fromInteger (-1))) <$> getCurrentTime)
          -- b1ExpiredKeyExpiryResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey b1ExpiredKeyExpiry)
          -- b1ExpiredKeyExpiryResult `shouldSatisfy` isLeft

          step "That it's possible to revoke a key"
          b1K3StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          b1K3StoredKeyIdResult `shouldSatisfy` isRight
          let b1K3StoredKeyId = right b1K3StoredKeyIdResult
          b1K3Now <- getCurrentTime
          b1K3RevokedResponce <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K3StoredKeyId)
          b1K3RevokedResponce `shouldSatisfy` isRight
          b1K3RevokedResponce `shouldSatisfy` (checkRecord (within1Second b1K3Now) getRevocationTime)

          step "That the key status updates after the key is revoked"
          b1K3RevokedInfoResponce <- http (getPublicKeyInfo b1K3StoredKeyId)
          b1K3RevokedInfoResponce `shouldSatisfy` isRight
          b1K3RevokedInfoResponce `shouldSatisfy` (checkRecord (Revoked ==) keyInfoState)

          step "That revoking an already revoked key generates an error"
          b1K3RevokedAgainResponce <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K3StoredKeyId)
          b1K3RevokedAgainResponce `shouldSatisfy` isLeft

          -- TODO: Include this test. (github #211)
          -- step "That another user from the same business can also revoke the key"
          -- b1K4StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          -- b1K4StoredKeyIdResult `shouldSatisfy` isRight
          -- let b1K4StoredKeyId = right b1K4StoredKeyIdResult
          -- b1K4RevokedResponce <- http (revokePublicKey (newUserToBasicAuthData userB1U2) b1K4StoredKeyId)
          -- b1K4RevokedResponce `shouldSatisfy` isRight
          -- b1K4RevokedInfoResponce <- http (getPublicKeyInfo b1K4StoredKeyId)
          -- b1K4RevokedInfoResponce `shouldSatisfy` isRight
          -- b1K4RevokedInfoResponce `shouldSatisfy` (checkRecord (Revoked ==) keyInfoState)

          step "That a user from the another business can't also revoke the key"
          b1K5StoredKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
          b1K5StoredKeyIdResult `shouldSatisfy` isRight
          let b1K5StoredKeyId = right b1K5StoredKeyIdResult
          b1K5RevokedResponce <- http (revokePublicKey (newUserToBasicAuthData userB2U1) b1K5StoredKeyId)
          b1K5RevokedResponce `shouldSatisfy` isLeft
          b1K5RevokedInfoResponce <- http (getPublicKeyInfo b1K5StoredKeyId)
          b1K5RevokedInfoResponce `shouldSatisfy` isRight
          b1K5RevokedInfoResponce `shouldSatisfy` (checkRecord (InEffect ==) keyInfoState)

          step "That revokePublicKey for an invalid keyId fails gracefully"
          revokeInvalidKeyIdResponce <- http (revokePublicKey (newUserToBasicAuthData userB1U1) (BRKeyId nil))
          revokeInvalidKeyIdResponce `shouldSatisfy` isLeft

          step "Test where the key has an expiry time (which hasn't expired) and is revoked reports the correct status."
          b1K6ExpiryUTC <- (addUTCTime (fromInteger expiryDelay)) <$> getCurrentTime
          let b1K6Expiry = (Just . ExpirationTime) b1K6ExpiryUTC
          b1K6StoreKeyIdResult <- http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey b1K6Expiry)
          b1K6StoreKeyIdResult `shouldSatisfy` isRight
          let b1K6KeyId = right b1K6StoreKeyIdResult
          b1K6RevokeKeyResult <- http (revokePublicKey (newUserToBasicAuthData userB1U1) b1K6KeyId)
          b1K6RevokeKeyResult `shouldSatisfy` isRight
          b1K6ExpiryRevokedResponce <- http (getPublicKeyInfo b1K6KeyId)
          -- This a test integrity check. We need to make sure that the time after we sent the request is not enough that the key will be after exipry time here.
          b1K6TimeAfterResponce <- getCurrentTime
          b1K6TimeAfterResponce `shouldSatisfy` (< b1K6ExpiryUTC)
          b1K6ExpiryRevokedResponce `shouldSatisfy` isRight
          b1K6ExpiryRevokedResponce `shouldSatisfy` (checkRecord (Revoked ==) keyInfoState)

          step "Test where the key has an expiry time and a revoked time which expired after it was revoked and both revoked and expired time have passed."
          -- Wait for the key from the previous test to expire and then recheck the status.
          threadDelay $ millisecondsToSeconds $ ceiling $ diffUTCTime b1K6ExpiryUTC b1K6TimeAfterResponce
          b1K6ExpiredRevokedResponce <- http (getPublicKeyInfo b1K6KeyId)
          b1K6ExpiredRevokedResponce `shouldSatisfy` isRight
          b1K6ExpiredRevokedResponce `shouldSatisfy` (checkRecord (Revoked ==) keyInfoState)



          -- Function to run a test predicate over all the keys in one of the test keys subdirectories.
          let testDirectory keyDirectory predicate = do
                let directory = "test" </> "Mirza" </> "Common" </> "testKeys" </> keyDirectory
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



shouldSatisfyIO :: (HasCallStack, Show a, Eq a) => IO a -> (a -> Bool) -> Expectation
action `shouldSatisfyIO` p = action >>= (`shouldSatisfy` p)

go :: GlobalOptions
go = GlobalOptions testDbConnStr 14 8 1 DebugS Dev

runApp :: IO (ThreadId, BaseUrl)
runApp = do
  ctx <- initBRContext go
  startWaiApp =<< initApplication go (RunServerOptions 8000) ctx

startWaiApp :: Wai.Application -> IO (ThreadId, BaseUrl)
startWaiApp app = do
    (prt, sock) <- openTestSocket
    let settings = setPort prt defaultSettings
    thread <- forkIO $ runSettingsSocket settings sock app
    return (thread, BaseUrl Http "localhost" prt "")

endWaiApp :: (ThreadId, BaseUrl) -> IO ()
endWaiApp (thread, _) = killThread thread

openTestSocket :: IO (Port, Socket)
openTestSocket = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  prt <- socketPort s
  return (fromIntegral prt, s)

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient :: BaseUrl -> ClientM a  -> IO (Either ServantError a)
runClient baseUrl' x = runClientM x (mkClientEnv manager' baseUrl')



-- *****************************************************************************
-- Test Utility Functions
-- *****************************************************************************

newBusinessToBusinessResponse :: NewBusiness -> BusinessResponse
newBusinessToBusinessResponse business = (BusinessResponse
                                          <$> newBusinessGS1CompanyPrefix
                                          <*> newBusinessName)
                                          business

newUserToBasicAuthData :: NewUser -> BasicAuthData
newUserToBasicAuthData =
  BasicAuthData
  <$> encodeUtf8 . getEmailAddress . newUserEmailAddress
  <*> encodeUtf8 . newUserPassword


-- Test helper function that enables a predicate to be run on the result of a test call.
checkRecord :: (a -> Bool) -> (b -> a) -> Either c b -> Bool
checkRecord predicate accessor (Right response) = predicate (accessor response)
checkRecord _         _        (Left _)         = False

-- Only use this from tests and only where you are sure that you will have a right.
right :: Either a b -> b
right (Right x) = x
right _         = error "Wasn't right..."


-- TODO: Move into test utils after merging with Sajid.
-- Checks that the two times are within 1 second of each other.
within1Second :: UTCTime -> UTCTime -> Bool
within1Second expected actual = abs (diffUTCTime expected actual) < (fromInteger 1)

-- TODO: This is copied from keys, move it into a higher level module and remove it from here and there after merging with sajid work.
readRsaPubKey :: FilePath -> IO PEM_RSAPubKey
readRsaPubKey filename = PEM_RSAPubKey <$> TIO.readFile filename

-- TODO: this is copied from keys, move it into a higher level module and remove it from here and there after merging with sajid work.
goodRsaPublicKey :: IO PEM_RSAPubKey
goodRsaPublicKey = readRsaPubKey "./test/Mirza/Common/testKeys/goodKeys/4096bit_rsa_key.pub"

millisecondsToSeconds :: (Num a) => a -> a
millisecondsToSeconds = (* 1000000)


bootstrapAuthData :: (HasEnvType w, HasConnPool w, HasKatipContext w, HasKatipLogEnv w, HasScryptParams w) => w -> IO BasicAuthData
bootstrapAuthData ctx = do
  -- We delibrately keep the domain @example.com so that the address doesn't potentially exist.
  globalUserEmail <- (<> "@example.com") <$> randomText
  -- We specifically prefix the password with "PlainTextPassword:" so that it makes it more obvious if this password
  -- shows up anywhere in plain text by mistake.
  globalUserPassword <- ("PlainTextPassword:" <>) <$> randomText
  let globalTestCompanyPrefix = (GS1CompanyPrefix "Tests Global Business Company Prefix")
  let globalTestsBusiness = NewBusiness globalTestCompanyPrefix "Tests Global Business Name"
  insertGlobalBusinessResult  <- runAppM @_ @BusinessRegistryError ctx $ BRHB.addBusiness globalTestsBusiness
  insertGlobalBusinessResult `shouldSatisfy` isRight
  let globalTestsUser = NewUser (EmailAddress globalUserEmail)
                              globalUserPassword
                              globalTestCompanyPrefix
                              "Tests Global User First Name"
                              "Tests Global User Last Name"
                              "Tests Global User Phone Number"
  insertGlobalUserResult <- runAppM @_ @BusinessRegistryError ctx $ runDb (BRHU.addUserQuery globalTestsUser)
  insertGlobalUserResult `shouldSatisfy` isRight

  return $ newUserToBasicAuthData globalTestsUser


randomText :: IO Text
randomText = do
  count <- randomRIO (8 :: Int, 32)
  randomString <- (take count) <$> replicateM count (randomRIO ('a', 'z'))
  return $ pack randomString