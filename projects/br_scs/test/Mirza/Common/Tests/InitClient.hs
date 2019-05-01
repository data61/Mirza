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
import           Mirza.Common.Utils                       (randomText, mockURI, expectUser)

import           Control.Concurrent                       (ThreadId)

import           Servant.Auth.Client                      (Token)
import           Servant.API.BasicAuth
import           Servant.Client                           (BaseUrl (..))

import           Data.Either                              (isRight)

import           Data.Text

import           Test.Tasty.Hspec

import           System.IO.Temp                           (emptySystemTempFile)

import           Katip                                    (Severity (DebugS))

import           Database.Beam.Query                      (delete, runDelete,
                                                           val_)

import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.BusinessRegistry.Client.Servant
import qualified Mirza.BusinessRegistry.Handlers.Business as BRHB (addBusiness)
import qualified Mirza.BusinessRegistry.Handlers.Users    as BRHU (addUserOnlyId)
import           Mirza.BusinessRegistry.Main              (ServerOptionsBR (..),
                                                           initBRContext, addAuthOptions)
import qualified Mirza.BusinessRegistry.Main              as BRMain
import           Mirza.BusinessRegistry.Types             as BT hiding (businessName)

import           Mirza.Common.Tests.Utils                 (DatabaseConnectionString (..),
                                                           DatabaseName (..),
                                                           databaseNameToConnectionString,
                                                           getDatabaseConnectionString)


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
  ServerOptionsSCS Dev False Nothing connectionString ("localhost", 8000) 14 8 1 DebugS (Just (brHost, brPrt)) where
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
  BusinessResponse <$> newBusinessGS1CompanyPrefix <*> newBusinessName <*> newBusinessUrl


newUserToBasicAuthData :: BT.NewUser -> BasicAuthData
newUserToBasicAuthData _newUser = BasicAuthData "" "" -- TODO: Extract info from or associated with newUser.


bootstrapAuthData :: (HasEnvType w, HasConnPool w, HasKatipContext w,
                      HasKatipLogEnv w)
                     => w -> IO Token
bootstrapAuthData ctx = do
  let user = BT.NewUser "initialUserOAuthSub"
  let prefix = GS1CompanyPrefix "1000000"
  let businessName = "Business Name"
      business = NewBusiness prefix businessName (mockURI businessName)
  insertUserResult <- runAppM @_ @BRError ctx $ BRHU.addUserOnlyId user
  insertUserResult `shouldSatisfy` isRight
  insertBusinessResult <- runAppM @_ @BRError ctx $ BRHB.addBusiness (expectUser $ insertUserResult) business
  insertBusinessResult `shouldSatisfy` isRight

  pure $ authDataToTokenTodoRemove $ newUserToBasicAuthData user

-- We specifically prefix the password with "PlainTextPassword:" so that it
-- makes it more obvious if this password shows up anywhere in plain text by
-- mistake.
randomPassword :: IO Text
randomPassword = ("PlainTextPassword:" <>) <$> randomText

brOptions :: Maybe FilePath -> ServerOptionsBR
brOptions mfp = ServerOptionsBR connectionString DebugS mfp Dev
  where
    connectionString = getDatabaseConnectionString testDbConnectionStringBR


runBRApp :: IO (ThreadId, BaseUrl, Token)
runBRApp = do
  tempFile <- emptySystemTempFile "businessRegistryTests.log"
  let currentBrOptions = brOptions (Just tempFile)
  minimalContext <- initBRContext currentBrOptions
  completeContext <- addAuthOptions minimalContext "" -- TODO: Use the proper oauth aud (audience) rathern then the empty text "".
  let BusinessRegistryDB businessesTable usersTable organisationMappingTable keysTable locationsTable geolocationsTable
        = businessRegistryDB

  flushDbResult <- runAppM @_ @BRError completeContext $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable geolocationsTable
      deleteTable locationsTable
      deleteTable keysTable
      deleteTable organisationMappingTable
      deleteTable usersTable
      deleteTable businessesTable
  flushDbResult `shouldSatisfy` isRight

  -- This construct somewhat destroys the integrity of these test since it is
  -- necessary to assume that these functions work correctly in order for the
  -- test cases to complete.
  token <- bootstrapAuthData completeContext

  (tid,brul) <- startWaiApp =<< BRMain.initApplication completeContext
  pure (tid, brul, token)

-- *****************************************************************************
-- Common Utility Functions
-- *****************************************************************************

data TestData = TestData
  { brThread   :: ThreadId
  , scsThread  :: ThreadId
  , brBaseUrl  :: BaseUrl
  , scsBaseUrl :: BaseUrl
  , brAuthData :: Token
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
