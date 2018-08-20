{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.BusinessRegistry.Tests.Client where

import           Mirza.BusinessRegistry.Tests.Settings  (testDbConnStr)

import           Control.Concurrent                     (ThreadId, forkIO,
                                                         killThread)
import           Control.Exception                      (bracket)
import           System.IO.Unsafe                       (unsafePerformIO)

import qualified Network.HTTP.Client                    as C
import           Network.Socket
import qualified Network.Wai                            as Wai
import           Network.Wai.Handler.Warp

import           Servant.API.BasicAuth
import           Servant.Client

import           Control.Monad                          (forM_)
import           Data.Either
import           Data.List                              (isSuffixOf)
import           Data.Text                              (Text)
import           Data.Text.Encoding                     (encodeUtf8)
import qualified Data.Text.IO                           as TIO
import           System.Directory                       (listDirectory)
import           System.FilePath                        ((</>))
import           System.IO                              (FilePath)

import           Test.Hspec.Expectations
import           Test.Tasty
import           Test.Tasty.HUnit

import           Database.Beam.Query
import           Mirza.BusinessRegistry.Client.Servant
import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.Main            (GlobalOptions (..),
                                                         RunServerOptions (..),
                                                         initApplication,
                                                         initBRContext)
import           Mirza.BusinessRegistry.Types


import           Data.GS1.EPC                           (GS1CompanyPrefix (..))


import           Katip                                  (Severity (InfoS))

-- Cribbed from https://github.com/haskell-servant/servant/blob/master/servant-client/test/Servant/ClientSpec.hs

-- === Servant Client tests

-- *****************************************************************************
-- Test Data
-- *****************************************************************************

userABC :: NewUser
userABC = NewUser
  { newUserPhoneNumber = "0400 111 222"
  , newUserEmailAddress = EmailAddress "abc@example.com"
  , newUserFirstName = "Johnny"
  , newUserLastName = "Smith"
  , newUserCompany = GS1CompanyPrefix "something"
  , newUserPassword = "re4lly$ecret14!"}

authABC :: BasicAuthData
authABC = BasicAuthData
  (encodeUtf8 . getEmailAddress . newUserEmailAddress $ userABC)
  (encodeUtf8 . newUserPassword                      $ userABC)


newBusinessToBusinessResponse :: NewBusiness -> BusinessResponse
newBusinessToBusinessResponse business = (BusinessResponse
                                          <$> newBusinessGs1CompanyPrefix
                                          <*> newBusinessName)
                                          business

makeNewBusiness :: GS1CompanyPrefix -> Text -> NewBusiness
makeNewBusiness prefix name = NewBusiness prefix name

newUserToBasicAuthData :: NewUser -> BasicAuthData
newUserToBasicAuthData =
  BasicAuthData
  <$> encodeUtf8 . getEmailAddress . newUserEmailAddress
  <*> encodeUtf8 . newUserPassword

-- todo this is copied from keys, move it into a higher level module and remove it from here and there.
rsaPubKey :: IO PEM_RSAPubKey
rsaPubKey = PEM_RSAPubKey <$> TIO.readFile "./test/Mirza/Common/testKeys/goodKeys/test.pub"


-- todo this is copied from keys, move it into a higher level module and remove it from here and there.
rsaPubKey' :: FilePath -> IO PEM_RSAPubKey
rsaPubKey' filename = PEM_RSAPubKey <$> TIO.readFile filename


clientSpec :: IO TestTree
clientSpec = do
  ctx <- initBRContext go
  let BusinessRegistryDB usersTable businessesTable keysTable
        = businessRegistryDB

  res <- runAppM @_ @BusinessRegistryError ctx $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable keysTable
      deleteTable usersTable
      deleteTable businessesTable

  res `shouldSatisfy` isRight

  let businessTests = testCaseSteps "Can create businesses" $ \step ->
        bracket runApp endWaiApp $ \(_tid,baseurl) -> do
          let http = runClient baseurl
              primaryBusiness = makeNewBusiness (GS1CompanyPrefix "businessTests_primaryCompanyPrefix") "businessTests_primaryBusinessName"
              primaryBusinessResponse = newBusinessToBusinessResponse primaryBusiness
              secondaryBusiness =  makeNewBusiness (GS1CompanyPrefix "businessTests_secondaryCompanyPrefix") "businessTests_secondaryBusinessName"
              secondaryBusinessResponse = newBusinessToBusinessResponse secondaryBusiness
              -- emptyCompanyPrefixBusiness =  makeNewBusiness (GS1CompanyPrefix "") "EmptyBusiness"


          step "Can create a new business"
          http (addBusiness primaryBusiness)
            `shouldSatisfyIO` isRight
          -- TODO: Check that the output is correct.

          step "That the added business was added and can be listed."
          http listBusiness >>=
            either (const $ expectationFailure "Error listing businesses")
                  (`shouldContain` [ primaryBusinessResponse])

          step "Can't add business with the same GS1CompanyPrefix"
          http (addBusiness primaryBusiness{newBusinessName = "businessTests_anotherName"})
            `shouldSatisfyIO` isLeft
          -- TODO: Check that the error type is correct / meaningful.

          step "Can add a second business"
          http (addBusiness secondaryBusiness)
            `shouldSatisfyIO` isRight

          step "List businesses returns all of the businesses"
          http listBusiness >>=
              either (const $ expectationFailure "Error listing businesses")
                    (`shouldContain` [ primaryBusinessResponse
                                        , secondaryBusinessResponse])

          -- step "That the GS1CompanyPrefix can't be empty (\"\")."
          -- http (addBusiness emptyCompanyPrefixBusiness)
          --   `shouldSatisfyIO` isLeft


  let userTests = testCaseSteps "Can create users" $ \step ->
        bracket runApp endWaiApp $ \(_tid,baseurl) -> do
          let http = runClient baseurl
              companyPrefix = (GS1CompanyPrefix "userTests_companyPrefix")
              business = makeNewBusiness companyPrefix "userTests_businessName"

          let user1 = NewUser (EmailAddress "userTests_email1@example.com") "password" companyPrefix "userTests First Name 1" "userTests Last Name 1" "userTests Phone Number 1"
              user2 = NewUser (EmailAddress "userTests_email2@example.com") "password" companyPrefix "userTests First Name 2" "userTests Last Name 2" "userTests Phone Number 2"
              userSameEmail = NewUser (newUserEmailAddress user1) "password" companyPrefix  "userTests First Name Same Email" "userTests Last Name Same Email" "userTests Phone Number Same Email" -- Same email address as user1 other fields different.
              userNonRegisteredBusiness = NewUser (EmailAddress "userTests_unregisteredBusiness@example.com") "password" (GS1CompanyPrefix "unregistered") "userTests First Name Unregistered Business" "userTests Last Name Unregistered Business" "userTests Phone Number Unregistered Business"
              -- userEmptyEmail = NewUser (EmailAddress "") "password" companyPrefix  "userTests First Name Empty Email" "userTests Last Name Empty Email" "userTests Phone Number Empty Email"
              -- userEmptyPassword = NewUser (EmailAddress "userTests_emptyPassword@example.com") "" companyPrefix  "userTests First Name Empty Password" "userTests Last Name Empty Password" "userTests Phone Number Empty Password"

          -- Create a business to use from further test cases (this is tested in
          --  the businesses tests so doesn't need to be explicitly tested here).
          _ <- http (addBusiness business)

          -- TODO add comment here
          goodKey <- rsaPubKey

          -- We delibrately test the "good user" that we will later add so that
          -- we know that we are failing because they aren't in the DB rather
          -- then because they are somehow otherwise invalid.
          step "That a user that doesn't exist can't login"
          http (addPublicKey (newUserToBasicAuthData user1) goodKey Nothing)
            `shouldSatisfyIO` isLeft
          putStrLn "can't log in"

          step "Can create a new user"
          http (addUser user1)
            `shouldSatisfyIO` isRight

          step "That the created user can login"
          http (addPublicKey (newUserToBasicAuthData user1) goodKey Nothing)
            `shouldSatisfyIO` isRight

          step "Can't create a new user with a GS1CompanyPrefix that isn't registered"
          res1 <- http (addUser userNonRegisteredBusiness)
          print res
          res1 `shouldSatisfy` isLeft

          step "Can't create a new user with the same email address"
          http (addUser userSameEmail)
            `shouldSatisfyIO` isLeft

          step "Can create a second user"
          http (addUser user2)
            `shouldSatisfyIO` isRight

          -- step "Can't create a user with an empty email."
          -- http (addUser userEmptyEmail)
          --   `shouldSatisfyIO` isLeft

          -- step "Can't create a user with an empty password."
          -- http (addUser userEmptyPassword)
          --   `shouldSatisfyIO` isLeft


  let keyTests = testCaseSteps "That keys work as expected" $ \step -> do
        bracket runApp endWaiApp $ \(_tid, baseurl) -> do
          step "PENDING"
          let http = runClient baseurl
              business1CompanyPrefix = (GS1CompanyPrefix "prefix1")
              business1 = makeNewBusiness business1CompanyPrefix "Name"

          let userB1U1 = NewUser (EmailAddress "keys1") "password" business1CompanyPrefix "" "" "" -- Business1User1
              -- userB1U2 = NewUser "" (EmailAddress "keys2") "" "" business1CompanyPrefix "" -- Business1User2
              -- userB2U1 = NewUser "" (EmailAddress "keys3") "" "" business2CompanyPrefix "" -- Business2User1
              -- userB2U2 = NewUser "" (EmailAddress "keys4") "" "" business2CompanyPrefix "" -- Business2User2

          -- Create a business to use from further test cases (this is tested in
          --  the businesses tests so doesn't need to be explicitly tested here).
          _ <- http (addBusiness business1)

          -- Create a business to use from further test cases (this is tested in
          --  the businesses tests so doesn't need to be explicitly tested here).
          _ <- http (addUser userB1U1)

          -- TODO add comment here
          goodKey <- rsaPubKey


          step "Can add a good key"
          http (addPublicKey (newUserToBasicAuthData userB1U1) goodKey Nothing)
            `shouldSatisfyIO` isRight


          let testDirectory keyType predicate = do
                let directory = "test" </> "Mirza" </> "Common" </> "testKeys" </> keyType
                files <- filter (".pub" `isSuffixOf`) <$> listDirectory directory
                let fullyQualifiedFiles = (directory </>) <$> files
                keys <- traverse rsaPubKey' fullyQualifiedFiles
                forM_ (zip files keys) $ \(keyName,key) -> do
                  step $ "Testing " ++ keyType ++ " key: " ++ keyName
                  http (addPublicKey (newUserToBasicAuthData userB1U1) key Nothing)
                    `shouldSatisfyIO` predicate

          step "Can add all of the good keys"
          testDirectory "goodKeys" isRight

          -- TODO: Change small keys to bad keys and rename the keys to the type of the key problem
          step "Can't add any of the bad keys"
          testDirectory "smallKeys" isLeft


  pure $ testGroup "Business Registry HTTP Client tests"
        [ businessTests
        , userTests
        , keyTests
        ]
-- |
-- @action \`shouldReturn\` expected@ sets the expectation that @action@
-- returns @expected@.
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
