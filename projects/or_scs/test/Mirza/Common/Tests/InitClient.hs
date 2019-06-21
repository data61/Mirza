{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.Common.Tests.InitClient where

import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.Main            (ServerOptionsSCS (..),
                                                    initSCSContext)
import qualified Mirza.SupplyChain.Main            as SCSMain
import           Mirza.SupplyChain.Types           as ST

import           Data.GS1.EPC                      (GS1CompanyPrefix (..))
import           Mirza.Common.Tests.ServantUtils
import           Mirza.Common.Utils                (expectUser, mockURI,
                                                    randomText)

import           Control.Concurrent                (ThreadId)

import           Servant.API.BasicAuth
import           Servant.Auth.Client               (Token)
import           Servant.Client                    (BaseUrl (..))

import           Data.Either                       (isRight)

import           Data.Text

import           Test.Tasty.Hspec

import           System.IO.Temp                    (emptySystemTempFile)

import           Katip                             (Severity (DebugS))

import           Database.Beam.Query               (delete, runDelete, val_)

import           Mirza.OrgRegistry.Client.Servant
import           Mirza.OrgRegistry.Database.Schema
import qualified Mirza.OrgRegistry.Handlers.Org    as ORHO (addOrg)
import qualified Mirza.OrgRegistry.Handlers.Users  as ORHU (addUserOnlyId)
import           Mirza.OrgRegistry.Main            (ServerOptionsOR (..),
                                                    addAuthOptions,
                                                    initORContext)
import qualified Mirza.OrgRegistry.Main            as ORMain
import           Mirza.OrgRegistry.Types           as ORT hiding (orgName)

import           Mirza.Common.Tests.Utils          (DatabaseConnectionString (..),
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
mkSoSCS (BaseUrl _ orHost orPrt _) =
  ServerOptionsSCS Dev False Nothing connectionString ("localhost", 8000) DebugS (Just (orHost, orPrt)) where
    connectionString = getDatabaseConnectionString testDbConnectionStringSCS

runSCSApp :: BaseUrl -> IO (ThreadId, BaseUrl)
runSCSApp orUrl = do
  tempFile <- emptySystemTempFile "supplyChainServerTests.log"
  let so' = mkSoSCS orUrl (Just tempFile)
  ctx <- initSCSContext so'
  let SupplyChainDb
        orgsTable
        labelsTable
        whatLabelsTable
        transformationsTable
        locationsTable
        eventsTable
        whatsTable
        orgTransactionsTable
        whysTable
        wheresTable
        whensTable
        labelEventsTable
        signaturesTable
        hashesTable
        blockchainTable
          = supplyChainDb
  flushDbResult <- runAppM @_ @ServiceError ctx $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable $ orgsTable
      deleteTable $ labelsTable
      deleteTable $ whatLabelsTable
      deleteTable $ transformationsTable
      deleteTable $ locationsTable
      deleteTable $ eventsTable
      deleteTable $ whatsTable
      deleteTable $ orgTransactionsTable
      deleteTable $ whysTable
      deleteTable $ wheresTable
      deleteTable $ whensTable
      deleteTable $ labelEventsTable
      deleteTable $ signaturesTable
      deleteTable $ hashesTable
      deleteTable $ blockchainTable
  flushDbResult `shouldSatisfy` isRight
  startWaiApp =<< SCSMain.initApplication so' ctx



-- *****************************************************************************
-- OR Utility Functions
-- *****************************************************************************

-- | Default database name when running tests for the OR. Be careful using this
-- construct as it could lead to problems...users not specifying the database
-- and accidentally operating on the wrong database.
-- See: testDbConnectionStringOR if you need a full connection string.
testDbNameOR :: DatabaseName
testDbNameOR = DatabaseName "testorgregistry"

-- | Default database connection string used when running tests for the OR. Be
-- careful using this construct as it could lead to problems...users not
-- specifying the database and accidentally operating on the wrong database.
testDbConnectionStringOR :: DatabaseConnectionString
testDbConnectionStringOR = databaseNameToConnectionString testDbNameOR


newOrgToOrgResponse :: NewOrg -> OrgResponse
newOrgToOrgResponse =
  OrgResponse <$> newOrgGS1CompanyPrefix <*> newOrgName <*> newOrgUrl


newUserToBasicAuthData :: ORT.NewUser -> BasicAuthData
newUserToBasicAuthData _newUser = BasicAuthData "" "" -- TODO: Extract info from or associated with newUser.


bootstrapAuthData :: (HasEnvType w, HasConnPool w, HasKatipContext w,
                      HasKatipLogEnv w)
                     => w -> IO Token
bootstrapAuthData ctx = do
  let user = ORT.NewUser "initialUserOAuthSub"
  let prefix = GS1CompanyPrefix "1000000"
  let orgName = "Org Name"
      org = NewOrg prefix orgName (mockURI orgName)
  insertUserResult <- runAppM @_ @ORError ctx $ ORHU.addUserOnlyId user
  insertUserResult `shouldSatisfy` isRight
  insertOrgResult <- runAppM @_ @ORError ctx $ ORHO.addOrg (expectUser $ insertUserResult) org
  insertOrgResult `shouldSatisfy` isRight

  pure $ authDataToTokenTodoRemove $ newUserToBasicAuthData user

-- We specifically prefix the password with "PlainTextPassword:" so that it
-- makes it more obvious if this password shows up anywhere in plain text by
-- mistake.
randomPassword :: IO Text
randomPassword = ("PlainTextPassword:" <>) <$> randomText

orOptions :: Maybe FilePath -> ServerOptionsOR
orOptions mfp = ServerOptionsOR connectionString DebugS mfp Dev
  where
    connectionString = getDatabaseConnectionString testDbConnectionStringOR


runORApp :: IO (ThreadId, BaseUrl, Token)
runORApp = do
  tempFile <- emptySystemTempFile "orgRegistryTests.log"
  let currentBrOptions = orOptions (Just tempFile)
  minimalContext <- initORContext currentBrOptions
  completeContext <- addAuthOptions minimalContext "my_fake_jwk_key" -- TODO: Use the proper oauth aud (audience)
  let OrgRegistryDB orgsTable usersTable orgMappingTable keysTable locationsTable geolocationsTable
        = orgRegistryDB

  flushDbResult <- runAppM @_ @ORError completeContext $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable geolocationsTable
      deleteTable locationsTable
      deleteTable keysTable
      deleteTable orgMappingTable
      deleteTable usersTable
      deleteTable orgsTable
  flushDbResult `shouldSatisfy` isRight

  -- This construct somewhat destroys the integrity of these test since it is
  -- necessary to assume that these functions work correctly in order for the
  -- test cases to complete.
  token <- bootstrapAuthData completeContext

  (tid,orul) <- startWaiApp =<< ORMain.initApplication completeContext
  pure (tid, orul, token)

-- *****************************************************************************
-- Common Utility Functions
-- *****************************************************************************

data TestData = TestData
  { orThread   :: ThreadId
  , scsThread  :: ThreadId
  , orBaseUrl  :: BaseUrl
  , scsBaseUrl :: BaseUrl
  , orAuthData :: Token
  }

endApps :: TestData -> IO ()
endApps (TestData orThreadId scsThreadId orUrl scsUrl _) = do
  endWaiApp (scsThreadId, scsUrl)
  endWaiApp (orThreadId, orUrl)

runApps :: IO TestData
runApps = do
  (orThreadId, orUrl, orAuthUser) <- runORApp
  (scsThreadId, scsUrl) <- runSCSApp orUrl
  pure $ TestData orThreadId scsThreadId orUrl scsUrl orAuthUser
