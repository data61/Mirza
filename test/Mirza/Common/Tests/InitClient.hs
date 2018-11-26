{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.Common.Tests.InitClient where

import           Mirza.SupplyChain.Database.Schema        as Schema
import           Mirza.SupplyChain.Main                   (ServerOptionsSCS (..),
                                                           initSCSContext)
import qualified Mirza.SupplyChain.Main                   as SCSMain
import           Mirza.SupplyChain.Types                  as ST

import           Data.GS1.EPC                             (GS1CompanyPrefix (..))
import           Mirza.Common.Tests.ServantUtils
import           Mirza.Common.Utils                       (randomText)

import           Control.Concurrent                       (ThreadId)

import           Servant.API.BasicAuth
import           Servant.Client                           (BaseUrl (..))

import           Data.Either                              (isRight)

import           Data.Text
import           Data.Text.Encoding                       (encodeUtf8)

import           Test.Tasty.Hspec

import           System.IO.Temp                           (emptySystemTempFile)

import           Katip                                    (Severity (DebugS))

import           Database.Beam.Query                      (delete, runDelete,
                                                           val_)

import           Mirza.BusinessRegistry.Database.Schema
import qualified Mirza.BusinessRegistry.Handlers.Business as BRHB (addBusiness)
import qualified Mirza.BusinessRegistry.Handlers.Users    as BRHU (addUserQuery)
import           Mirza.BusinessRegistry.Main              (RunServerOptions (..),
                                                           ServerOptionsBR (..),
                                                           initBRContext)
import qualified Mirza.BusinessRegistry.Main              as BRMain
import           Mirza.BusinessRegistry.Types             as BT

import           Mirza.Common.Tests.Utils                 (DatabaseConnectionString (..),
                                                           DatabaseName (..),
                                                           databaseNameToConnectionString,
                                                           getDatabaseConnectionString,
                                                           unsafeMkEmailAddress)
import           Text.Email.Validate                      (toByteString)

-- *****************************************************************************
-- SCS Utility Functions
-- *****************************************************************************

-- | Default database name when running tests for the SCS. Be careful using this
-- construct as it could lead to problems...users not specifying the database
-- and accidentally operating on the wrong database.
-- See: testDbConnectionStringSCS if you need a full connection string.
testDbNameSCS :: DatabaseName
testDbNameSCS = DatabaseName "testsupplychainserver"

-- | Default database connection string used when running tests for the SCS. Be
-- careful using this construct as it could lead to problems...users not
-- specifying the database and accidentally operating on the wrong database.
testDbConnectionStringSCS :: DatabaseConnectionString
testDbConnectionStringSCS = databaseNameToConnectionString testDbNameSCS

mkSoSCS :: BaseUrl -> Maybe FilePath -> ServerOptionsSCS
mkSoSCS (BaseUrl _ brHost brPrt _) =
  ServerOptionsSCS Dev False connectionString "127.0.0.1" 8000 14 8 1 DebugS brHost brPrt where
    connectionString = getDatabaseConnectionString testDbConnectionStringSCS

runSCSApp :: BaseUrl -> IO (ThreadId, BaseUrl)
runSCSApp brUrl = do
  tempFile <- emptySystemTempFile "supplyChainServerTests.log"
  let so' = mkSoSCS brUrl (Just tempFile)
  ctx <- initSCSContext so'
  let SupplyChainDb
        usersTable
        businessesTable
        contactsTable
        labelsTable
        whatLabelsTable
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
  startWaiApp =<< SCSMain.initApplication so' ctx



-- *****************************************************************************
-- BR Utility Functions
-- *****************************************************************************

-- | Default database name when running tests for the BR. Be careful using this
-- construct as it could lead to problems...users not specifying the database
-- and accidentally operating on the wrong database.
-- See: testDbConnectionStringBR if you need a full connection string.
testDbNameBR :: DatabaseName
testDbNameBR = DatabaseName "testbusinessregistry"

-- | Default database connection string used when running tests for the BR. Be
-- careful using this construct as it could lead to problems...users not
-- specifying the database and accidentally operating on the wrong database.
testDbConnectionStringBR :: DatabaseConnectionString
testDbConnectionStringBR = databaseNameToConnectionString testDbNameBR


newBusinessToBusinessResponse :: NewBusiness -> BusinessResponse
newBusinessToBusinessResponse =
  BusinessResponse <$> newBusinessGS1CompanyPrefix <*> newBusinessName


newUserToBasicAuthData :: BT.NewUser -> BasicAuthData
newUserToBasicAuthData =
  BasicAuthData
  <$> toByteString . BT.newUserEmailAddress
  <*> encodeUtf8 . BT.newUserPassword


bootstrapAuthData :: (HasEnvType w, HasConnPool w, HasKatipContext w,
                      HasKatipLogEnv w, HasScryptParams w)
                     => w -> IO BasicAuthData
bootstrapAuthData ctx = do
  let email = "initialUser@example.com"
  password <- randomPassword
  let prefix = GS1CompanyPrefix "1000000"
  let business = NewBusiness prefix "Business Name"
  insertBusinessResult  <- runAppM @_ @BRError ctx $ BRHB.addBusiness business
  insertBusinessResult `shouldSatisfy` isRight
  let user = BT.NewUser  (unsafeMkEmailAddress email)
                      password
                      prefix
                      "Test User First Name"
                      "Test User Last Name"
                      "Test User Phone Number"
  insertUserResult <- runAppM @_ @BRError ctx $ runDb (BRHU.addUserQuery user)
  insertUserResult `shouldSatisfy` isRight

  pure $ newUserToBasicAuthData user

-- We specifically prefix the password with "PlainTextPassword:" so that it
-- makes it more obvious if this password shows up anywhere in plain text by
-- mistake.
randomPassword :: IO Text
randomPassword = ("PlainTextPassword:" <>) <$> randomText

brOptions :: Maybe FilePath -> ServerOptionsBR
brOptions mfp = ServerOptionsBR connectionString 14 8 1 DebugS mfp Dev where
  connectionString = getDatabaseConnectionString testDbConnectionStringBR


runBRApp :: IO (ThreadId, BaseUrl, BasicAuthData)
runBRApp = do
  tempFile <- emptySystemTempFile "businessRegistryTests.log"
  let currentBrOptions = brOptions (Just tempFile)
  ctx <- initBRContext currentBrOptions
  let BusinessRegistryDB usersTable businessesTable keysTable locationsTable geolocationsTable
        = businessRegistryDB

  flushDbResult <- runAppM @_ @BRError ctx $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable keysTable
      deleteTable usersTable
      deleteTable businessesTable
      deleteTable locationsTable
      deleteTable geolocationsTable
  flushDbResult `shouldSatisfy` isRight

  -- This construct somewhat destroys the integrity of these test since it is
  -- necessary to assume that these functions work correctly in order for the
  -- test cases to complete.
  brAuthUser <- bootstrapAuthData ctx

  (tid,brul) <- startWaiApp =<< BRMain.initApplication currentBrOptions (RunServerOptions 8000) ctx
  pure (tid,brul,brAuthUser)

-- *****************************************************************************
-- Common Utility Functions
-- *****************************************************************************

data TestData = TestData
  { brThread   :: ThreadId
  , scsThread  :: ThreadId
  , brBaseUrl  :: BaseUrl
  , scsBaseUrl :: BaseUrl
  , brAuthData :: BasicAuthData
  }

endApps :: TestData -> IO ()
endApps (TestData brThreadId scsThreadId brUrl scsUrl _) = do
  endWaiApp (scsThreadId, scsUrl)
  endWaiApp (brThreadId, brUrl)

runApps :: IO TestData
runApps = do
  (brThreadId, brUrl, brAuthUser) <- runBRApp
  (scsThreadId, scsUrl) <- runSCSApp brUrl
  pure $ TestData brThreadId scsThreadId brUrl scsUrl brAuthUser
