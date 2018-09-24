{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Mirza.Common.Tests.InitClient where

import           Mirza.SupplyChain.Database.Schema        as Schema
import           Mirza.SupplyChain.Main                   (ServerOptions (..),
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

import           Data.ByteString.Char8                    (ByteString)
import           Data.Text
import           Data.Text.Encoding                       (encodeUtf8)

import           Test.Tasty.Hspec

import           Katip                                    (Severity (DebugS))

import           Database.Beam.Query                      (delete, runDelete,
                                                           val_)

import           Mirza.BusinessRegistry.Database.Schema
import qualified Mirza.BusinessRegistry.Handlers.Business as BRHB (addBusiness)
import qualified Mirza.BusinessRegistry.Handlers.Users    as BRHU (addUserQuery)
import           Mirza.BusinessRegistry.Main              (GlobalOptions (..),
                                                           RunServerOptions (..),
                                                           initBRContext)
import qualified Mirza.BusinessRegistry.Main              as BRMain
import           Mirza.BusinessRegistry.Types             as BT

-- *****************************************************************************
-- SCS Utility Functions
-- *****************************************************************************

testDbConnStrSCS :: ByteString
testDbConnStrSCS = "dbname=testsupplychainserver"

mkSoSCS :: BaseUrl -> ServerOptions
mkSoSCS (BaseUrl _ brHost brPrt _) =
    ServerOptions Dev False testDbConnStrSCS "127.0.0.1" 8000 14 8 1 DebugS brHost brPrt

runSCSApp :: BaseUrl -> IO (ThreadId, BaseUrl)
runSCSApp brUrl = do
  let soSCS = mkSoSCS brUrl
  ctx <- initSCSContext soSCS
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
  startWaiApp =<< SCSMain.initApplication soSCS ctx

-- *****************************************************************************
-- BR Utility Functions
-- *****************************************************************************
testDbConnStrBR :: ByteString
testDbConnStrBR = "dbname=testmirzabusinessregistry"

newBusinessToBusinessResponse :: NewBusiness -> BusinessResponse
newBusinessToBusinessResponse =
  BusinessResponse <$> newBusinessGS1CompanyPrefix <*> newBusinessName


newUserToBasicAuthData :: BT.NewUser -> BasicAuthData
newUserToBasicAuthData =
  BasicAuthData
  <$> encodeUtf8 . getEmailAddress . BT.newUserEmailAddress
  <*> encodeUtf8 . BT.newUserPassword


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
  let user = BT.NewUser  (EmailAddress email)
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

go :: GlobalOptions
go = GlobalOptions testDbConnStrBR 14 8 1 DebugS Dev

runBRApp :: IO (ThreadId, BaseUrl, BasicAuthData)
runBRApp = do
  ctx <- initBRContext go
  let BusinessRegistryDB usersTable businessesTable keysTable locationsTable
        = businessRegistryDB

  flushDbResult <- runAppM @_ @BusinessRegistryError ctx $ runDb $ do
      let deleteTable table = pg $ runDelete $ delete table (const (val_ True))
      deleteTable keysTable
      deleteTable usersTable
      deleteTable businessesTable
      deleteTable locationsTable
  flushDbResult `shouldSatisfy` isRight

  -- This construct somewhat destroys the integrity of these test since it is
  -- necessary to assume that these functions work correctly in order for the
  -- test cases to complete.
  globalAuthData <- bootstrapAuthData ctx

  (tid,brul) <- startWaiApp =<< BRMain.initApplication go (RunServerOptions 8000) ctx
  pure (tid,brul,globalAuthData)

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
  _ <- endWaiApp (scsThreadId, scsUrl)
  endWaiApp (brThreadId, brUrl)

runApps :: IO TestData
runApps = do
  (brThreadId, brUrl, brAuth) <- runBRApp
  (scsThreadId, scsUrl) <- runSCSApp brUrl
  return $ TestData brThreadId scsThreadId brUrl scsUrl brAuth
