{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Mirza.Common.Tests.InitClient where

import           Mirza.OrgRegistry.Database.Schema
import           Mirza.OrgRegistry.Main            (ServerOptionsOR (..),
                                                    addAuthOptions,
                                                    initORContext)
import qualified Mirza.OrgRegistry.Main            as ORMain
import           Mirza.OrgRegistry.Types           as ORT

import           Mirza.SupplyChain.Database.Schema as Schema
import           Mirza.SupplyChain.Main            (ServerOptionsSCS (..),
                                                    initSCSContext)
import qualified Mirza.SupplyChain.Main            as SCSMain
import           Mirza.SupplyChain.Types           as ST

import           Mirza.Common.Tests.ServantUtils
import           Mirza.Common.Tests.Utils          (DatabaseConnectionString (..),
                                                    DatabaseName (..),
                                                    databaseNameToConnectionString,
                                                    getDatabaseConnectionString)

import           Katip                             (Severity (DebugS))

import           Test.Tasty.Hspec

import           Database.Beam.Query               (delete, runDelete, val_)

import           Servant.Auth.Client               (Token (..))
import           Servant.Client                    (BaseUrl (..))

import           System.IO.Temp                    (emptySystemTempFile)

import           Crypto.JWT                        hiding (alg, header, jwk,
                                                    signClaims)
import qualified Crypto.JWT                        (signClaims)

import           Control.Concurrent                (ThreadId)
import           Control.Lens
import           Control.Monad.Except

import           Data.Aeson
import           Data.ByteString.Lazy.Char8
import           Data.Either                       (isRight)
import           Data.Text
--import           Data.Time.Clock
import           Network.URI



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


orOptions :: Maybe FilePath -> ServerOptionsOR
orOptions mfp = ServerOptionsOR connectionString DebugS mfp Dev
  where
    connectionString = getDatabaseConnectionString testDbConnectionStringOR


runORApp :: IO (ThreadId, BaseUrl, TokenTestSuite)
runORApp = do
  let jwksFilename :: [Char]
      jwksFilename = "test_auth_key.json"
      jwksFilenameURI :: URI
      jwksFilenameURI = URI "file:" (Just $ URIAuth "" "" "") jwksFilename "" ""
      oAuthSub :: Text
      oAuthSub = "TestOAuthSubject"
      oAuthAud :: Text
      oAuthAud = "TestOAuthAudience"

  tokenTestSuite <- makeTokenTestData jwksFilename oAuthSub oAuthAud
  tempFile <- emptySystemTempFile "orgRegistryTests.log"
  let currentOrOptions = orOptions (Just tempFile)
  minimalContext <- initORContext currentOrOptions
  completeContext <- addAuthOptions minimalContext jwksFilenameURI oAuthAud
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

  (tid,orul) <- startWaiApp =<< ORMain.initApplication completeContext
  pure (tid, orul, tokenTestSuite)



-- *****************************************************************************
-- Common Utility Functions
-- *****************************************************************************

data TestData = TestData
  { orThread        :: ThreadId
  , scsThread       :: ThreadId
  , orBaseUrl       :: BaseUrl
  , scsBaseUrl      :: BaseUrl
  , orTestTokenData :: TokenTestSuite
  }

endApps :: TestData -> IO ()
endApps (TestData orThreadId scsThreadId orUrl scsUrl _) = do
  endWaiApp (scsThreadId, scsUrl)
  endWaiApp (orThreadId, orUrl)

runApps :: IO TestData
runApps = do
  (orThreadId, orUrl, token) <- runORApp
  (scsThreadId, scsUrl) <- runSCSApp orUrl
  pure $ TestData orThreadId scsThreadId orUrl scsUrl token



-- *****************************************************************************
-- Auth / Token Utility Functions
-- *****************************************************************************

data TokenTestSuite = TokenTestSuite
  { testToken              :: Token
  , testTokenDefaultClaims :: ClaimsSet
  , testSignTokenClaims    :: ClaimsSet -> IO (Token)
  }


makeTokenTestData :: [Char] -> Text -> Text -> IO (TokenTestSuite)
makeTokenTestData jwksFilename oAuthSub oAuthAud = do
  key <- generateAndStoreTestJWK jwksFilename
  claims <- makeClaims oAuthSub oAuthAud
  buildTokenTestSuite claims (claimsToToken key)


generateAndStoreTestJWK :: [Char] -> IO JWK
generateAndStoreTestJWK filename = do
  jwk <- genJWK (RSAGenParam (4096 `div` 8))
  -- Here we write out the public and private key info, while we only need to have the public key and only make use of
  -- the private key there is no real point to removing it since it is valid only for as long as the test execution
  -- persists and it might be useful to have the private key for debugging key issues after the tests have completed if
  -- needed (can remove the private key in the future if reasons to remove trump the debugging potential).
  _ <- encodeFile filename jwk
  pure jwk


makeClaims :: Text -> Text -> IO ClaimsSet
makeClaims oAuthSub oAuthAud = do
  --time <- getCurrentTime
  pure $ emptyClaimsSet
    & claimAud .~ Just (Audience [review string oAuthAud])
    & claimSub .~ Just (review string oAuthSub)
  --    & claimIat .~ Just (NumericDate time)
  --    & claimExp .~ Just (NumericDate (addUTCTime (nominalDay) time))


claimsToToken :: JWK -> ClaimsSet -> IO (Token)
claimsToToken key claims = do
  let signFunction = signClaims key
  -- Note ideal that we just assume right here, but these are just tests so if we hit lefts we can always handle better
  -- then.
  (Right signedJWT) <- signFunction claims
  let tokenString = encodeCompact signedJWT
  pure $ (Token $ toStrict tokenString)


signClaims :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signClaims key claims = runExceptT $ do
  -- It's not ideal that we just fill this value rather then checking the key, but this seems simple and easy because
  -- these are just tests (anything more would be overkill).
  let algorithm = RS256
  let header = newJWSHeader ((), algorithm)
  Crypto.JWT.signClaims key header claims


buildTokenTestSuite :: ClaimsSet -> (ClaimsSet -> IO (Token)) -> IO (TokenTestSuite)
buildTokenTestSuite claims signFunction = do
  token <- signFunction claims
  pure $ TokenTestSuite token claims signFunction
