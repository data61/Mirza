{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Mirza.OrgRegistry.Main where


import           Mirza.Common.Types                 as CT
import           Mirza.OrgRegistry.API              (API, ServerAPI, api)
import           Mirza.OrgRegistry.Auth
import           Mirza.OrgRegistry.Database.Migrate
import           Mirza.OrgRegistry.Database.Schema  as Schema
import           Mirza.OrgRegistry.GenerateUtils    (dummyOrg, dummyUser)
import           Mirza.OrgRegistry.Service
import           Mirza.OrgRegistry.Types            as ORT
import           Mirza.Common.Utils (fetchJWKS)

import           Data.GS1.EPC                       (GS1CompanyPrefix (..))

import           Servant
import           Servant.Auth.Server
import           Servant.Swagger.UI

import           Network.URI                        hiding (path, authority)

import           Crypto.JOSE                        (JWK, JWKSet (..))
import           Crypto.JWT                         (Audience (..), string)

import qualified Data.Pool                          as Pool
import           Database.PostgreSQL.Simple

import           Network.URI                        (nullURI)
import           Network.Wai                        (Middleware)
import qualified Network.Wai.Handler.Warp           as Warp
import qualified Network.Wai.Middleware.Cors        as CorsMiddleware

import           Data.Aeson                         (eitherDecodeFileStrict)

import qualified Data.Attoparsec.ByteString         as A
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8              as BS
import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text, pack)
import           Options.Applicative                hiding (action)
import           Text.Email.Parser                  (addrSpec)

import           Control.Exception                  (finally)
import           Control.Lens                       (review)
import           Data.Either                        (fromRight)
import           Data.Maybe                         (fromMaybe, listToMaybe)
import           Katip                              as K
import           System.IO                          (IOMode (AppendMode),
                                                     hPutStr, openFile, stderr,
                                                     stdout, FilePath)


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Port number changed so that OR and SCS can be run at the same time
defaultPortNumber :: Int
defaultPortNumber = 8200

defaultDatabaseConnectionString :: ByteString
defaultDatabaseConnectionString = "dbname=devorgregistry"

-- | default JOSE Web Key Set URI
defaultJWKSURI :: URI
defaultJWKSURI = URI "https:" (Just $ URIAuth "" "mirza.au.auth0.com/.well-known" "") "/jwks.json" "" ""

corsOrigins :: [CorsMiddleware.Origin]
corsOrigins = [
    "http://localhost:8000"
  , "http://localhost:8020"
  , "http://localhost:8080"
  , "http://localhost:8081"
  , "http://localhost:8200"
  , "https://demo.mirza.d61.io"
  ]

--------------------------------------------------------------------------------
-- Command Line Options Data Types
--------------------------------------------------------------------------------
data InitOptionsOR = InitOptionsOR
  { soGlobals  :: ServerOptionsOR
  , soExecMode :: ExecMode
  }

data ExecMode
  = RunServer RunServerOptions
  | InitDb
  | UserAction UserCommand
  | OrgAction OrgCommand
  | PopulateDatabase  -- TODO: This option should be removed....this is for testing and debugging only.
  | Bootstrap Text GS1CompanyPrefix

data ServerOptionsOR = ServerOptionsOR
  { mandatoryOptionsDbConnStr    :: ByteString
  , mandatoryOptionsLoggingLevel :: K.Severity
  , mandatoryOptionsLogLocation  :: Maybe FilePath
  , mandatoryOptionsEnvType      :: CT.EnvType
  }

data RunServerOptions = RunServerOptions
  { runServerOptionsPortNumber    :: Int
  , runServerOptionsOAuthJWKPath  :: URI
  , runServerOptionsOAuthAudience :: Text
  }

data UserCommand
  = UserAdd
  | UserList

data OrgCommand
  = OrgAdd
  | OrgList


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = multiplexInitOptions =<< execParser opts where
  opts = Options.Applicative.info (serverOptions <**> helper)
    (fullDesc
    <> progDesc "Here to meet all your org registry needs"
    <> header "Supply Chain Org Registry Service")


-- Handles the overriding server options (this effectively defines the point
-- where the single binary could be split into multiple binaries.
multiplexInitOptions :: InitOptionsOR -> IO ()
multiplexInitOptions (InitOptionsOR opts mode) = case mode of
  RunServer rsOpts                       -> launchServer opts rsOpts
  InitDb                                 -> runMigration opts
  UserAction uc                          -> runUserCommand opts uc
  OrgAction bc                      -> runOrgCommand opts bc
  PopulateDatabase                       -> runPopulateDatabase opts
  Bootstrap oAuthSubSuffix companyPrefix -> runBootstrap opts oAuthSubSuffix companyPrefix


--------------------------------------------------------------------------------
-- Service
--------------------------------------------------------------------------------

launchServer :: ServerOptionsOR -> RunServerOptions -> IO ()
launchServer opts rso = do
      let portNumber = runServerOptionsPortNumber rso
      minimalContext <- initORContext opts
      completeContext <- addServerOptions minimalContext rso
      app <- initApplication completeContext
      mids <- initMiddleware opts rso
      putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/"
      Warp.run (fromIntegral portNumber) (mids app) `finally` closeScribes (ORT._orKatipLogEnv completeContext)


addServerOptions :: ORContextMinimal -> RunServerOptions -> IO ORContextComplete
addServerOptions minimalContext (RunServerOptions _port oAuthPublicKeyRef oauthAudience) = addAuthOptions minimalContext oAuthPublicKeyRef oauthAudience


addAuthOptions :: ORContextMinimal -> URI -> Text -> IO ORContextComplete
addAuthOptions minimalContext oAuthPublicKeyRef oauthAudience = do
  jwks <- getJWKS oAuthPublicKeyRef
  let audience = Audience [review string oauthAudience]
  pure $ orContextComplete minimalContext audience jwks


getJWKS :: URI -> IO JWK
getJWKS (URI "file:" authority path _ _) = do
  eitherJwk <- eitherDecodeFileStrict $ (maybe "" uriRegName authority) <> path
  let makeError errorMessage = error $ "Unable to get the OAuth Public Key. Error was: " <> (show errorMessage)
  let jwk = either makeError id eitherJwk
  pure jwk
getJWKS uri@(URI "https:" _ _ _ _) = do
  let uriString = uriToString id uri ""
      unableToFetchJWKError = error $ "Unable to fetch Jose Web Key Set from: " <> uriString
  keyResult <- fetchJWKS $ uriString
  case keyResult of
    Left _ -> unableToFetchJWKError
    Right (JWKSet keys) -> maybe unableToFetchJWKError pure $ listToMaybe keys
getJWKS _ = error $ "Unsupported URI schema type."




initORContext :: ServerOptionsOR -> IO ORT.ORContextMinimal
initORContext (ServerOptionsOR dbConnStr lev mlogPath envT) = do
  logHandle <- maybe (pure stdout) (flip openFile AppendMode) mlogPath
  hPutStr stderr $ "(Logging will be to: " ++ fromMaybe "stdout" mlogPath ++ ") "
  handleScribe <- mkHandleScribe ColorIfTerminal logHandle lev V3
  logEnv <- initLogEnv "orgRegistry" (Environment . pack . show $ envT)
            >>= registerScribe "stdout" handleScribe defaultScribeSettings
  connpool <- Pool.createPool (connectPostgreSQL dbConnStr) close
                      1 -- Number of "sub-pools",
                      60 -- How long in seconds to keep a connection open for reuse
                      10 -- Max number of connections to have open at any one time
                      -- TODO: Make this a config paramete
  pure $ orContextMinimal envT connpool logEnv mempty mempty


initApplication :: ORContextComplete -> IO Application
initApplication ev =
  pure $ serveWithContext api
          (tokenServerContext ev)
          (server ev)


myCors :: Middleware
myCors = CorsMiddleware.cors (const $ Just policy)
    where
      policy = CorsMiddleware.simpleCorsResourcePolicy
        { CorsMiddleware.corsRequestHeaders = ["Content-Type", "Authorization"]
        , CorsMiddleware.corsMethods = "PUT" : CorsMiddleware.simpleMethods
        , CorsMiddleware.corsOrigins = Just (corsOrigins, True)
        }

initMiddleware :: ServerOptionsOR -> RunServerOptions -> IO Middleware
initMiddleware _ _ = pure myCors

-- Implementation
server :: ORContextComplete -> Server API
server ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[CookieSettings, JWTSettings])
        (appMToHandler ev)
        (appHandlers @ORContextComplete @ORError)


--------------------------------------------------------------------------------
-- Migration Command
--------------------------------------------------------------------------------

runMigration :: ServerOptionsOR -> IO ()
runMigration opts = do
  ctx <- initORContext opts
  res <- runMigrationWithConfirmation @ORContextMinimal @SqlError ctx interactiveMigrationConfirm
  print res


--------------------------------------------------------------------------------
-- User Command
--------------------------------------------------------------------------------

runUserCommand :: ServerOptionsOR -> UserCommand -> IO ()
runUserCommand opts UserList = do
   ctx <- initORContext opts
   euser <- runAppM ctx $ runDb listUsersQuery
   either (print @ORError) (mapM_ print) euser

runUserCommand opts UserAdd = do
  user <- interactivelyGetNewUser
  ctx <- initORContext opts
  euser <- runAppM ctx $ runDb (addUserQuery user)
  either (print @ORError) print euser


interactivelyGetNewUser :: IO NewUser
interactivelyGetNewUser = do
  newUserOAuthSub     <- pack <$> prompt "OAuthSub:"
  pure NewUser{..}


--------------------------------------------------------------------------------
-- Org Command
--------------------------------------------------------------------------------

runOrgCommand :: ServerOptionsOR -> OrgCommand -> IO ()
runOrgCommand opts OrgList = do
  ctx <- initORContext opts
  eorgs <- runAppM ctx $ runDb (searchOrgsQuery Nothing Nothing Nothing)
  either (print @ORError) (mapM_ print) eorgs

runOrgCommand opts OrgAdd = do
  org <- interactivelyGetOrgT
  ctx <- initORContext opts
  eorg <- runAppM ctx $ runDb (addOrgQuery org)
  either (print @ORError) print eorg


interactivelyGetOrgT :: IO Org
interactivelyGetOrgT = do
  org_gs1_company_prefix <- GS1CompanyPrefix . pack <$>  prompt "GS1CompanyPrefix:"
  org_name               <- pack <$> prompt "Name:"
  org_url                <- pack <$> prompt "Url:"
  let org_last_update = Nothing
  pure OrgT{..}

prompt :: String -> IO String
prompt message = putStrLn message *> getLine


--------------------------------------------------------------------------------
-- Populate Database Command : TODO: This is for testing and debugging only and should be removed.
--------------------------------------------------------------------------------

runPopulateDatabase :: ServerOptionsOR -> IO ()
runPopulateDatabase opts = do
  context     <- initORContext opts
  let runWithContext = runAppM @_ @ORError context
  let right = fromRight (error "Error inserting user.")

  let b1u1 = dummyUser "B1U1"
  b1u1Result <- runWithContext $ addUserOnlyId b1u1
  let b1  =  dummyOrg "1"
  _result <- runWithContext $ addOrg (right b1u1Result) b1
  let b1u2 = dummyUser "B1U2"
  b1u2Result <- runWithContext $ addUserOnlyId b1u2
  _result <- runWithContext $ addOrganisationMapping (newOrgGS1CompanyPrefix b1) (right b1u2Result)

  let b2u1 = dummyUser "B2U1"
  b2u1Result <- runWithContext $ addUserOnlyId b2u1
  let b2  =  dummyOrg "2"
  _result <- runWithContext $ addOrg (right b2u1Result) b2
  let b2u2 = dummyUser "B2U2"
  b2u2Result <- runWithContext $ addUserOnlyId b2u2
  _result <- runWithContext $ addOrganisationMapping (newOrgGS1CompanyPrefix b2) (right b2u2Result)

  putStrLn "Inserted Orgs and Users Information"
  print b1
  print b1u1
  print b1u2

  print b2
  print b2u1
  print b2u2



--------------------------------------------------------------------------------
-- Bootstrap Command
--------------------------------------------------------------------------------

-- TODO: Remove this. This functionality is now redundant with our new user
-- model, but leaving this here for now because we still have some unresolved
-- user issues and so will leave this until we are sure that there is no longer
-- any use for it whatsoever.
-- This command is a bit of a hack. We need it so that we can noninteractively
-- add a user to the database so that this user can authenticate over the API
-- and add other users and orgs into the database. The fact that this user
-- doesn't take proper credentials suggests that our user model is wrong
-- (incomplete)... i.e. we need users that aren't associated with orgs,
-- but we need to do much more work here when we deal with permssions in general.
runBootstrap :: ServerOptionsOR -> Text -> GS1CompanyPrefix -> IO ()
runBootstrap opts oAuthSub companyPrefix = do
  let newUser = bootstrapUser oAuthSub
  let newOrg = bootstrapOrg companyPrefix

  context <- initORContext opts

  userResult <- runAppM @_ @ORError context $ addUser newUser
  either (print @ORError) print userResult

  -- We ignore the org insert result, for now we just try best effort.
  -- Can always improve the check and error handling here later if we need to
  -- improve the reliability or error reporting.
  case userResult of
      Right user -> do
                    orgResult <- runAppM @_ @ORError context $ addOrg (CT.UserId $ user_id user) newOrg
                    either (print @ORError) print orgResult
      Left _     -> putStrLn "Error inserting user, skipping adding org."

  where
    bootstrapUser :: Text -> NewUser
    bootstrapUser oAuthSubSuffix = do
      let newUserOAuthSub     = "bootstrapped-user-oauth-sub-" <> oAuthSubSuffix
      NewUser{..}

    bootstrapOrg :: GS1CompanyPrefix -> NewOrg
    bootstrapOrg prefix = do
      let newOrgGS1CompanyPrefix = prefix
      let newOrgName             = "Bootstrapped Org"
      let newOrgUrl              = nullURI
      NewOrg{..}

--------------------------------------------------------------------------------
-- Command Line Options Argument Parsers
--------------------------------------------------------------------------------

standardCommand :: String -> Parser a -> String -> Mod CommandFields a
standardCommand name action desciption =
  command name (info (action <**> helper) (progDesc desciption))


-- The standard format of the main command line options is [Command] [Action], this applies to things like org and user.
serverOptions :: Parser InitOptionsOR
serverOptions = InitOptionsOR
  <$> parsedServerOptions
  <*> subparser
        ( mconcat
          [ standardCommand "server"    runServer "Run HTTP server"
          , standardCommand "initdb"    initDb "Initialise the Database (Note: This command only works if the database \
                                               \is empty and can't be used for migrations or if the database already \
                                               \contains the schema."
          , standardCommand "user"      userCommand "Interactively add new users"
          , standardCommand "org"  orgCommand "Operations on orgs"
          , standardCommand "populate"  populateDb "Populate the database with dummy test data"
          , standardCommand "bootstrap" bootstrap "Bootstrap a user into the database."
          ]
        )


runServer :: Parser ExecMode
runServer = RunServer <$> (RunServerOptions
  <$> option auto
    (
       long "port"
    <> help "Port to run the service on."
    <> showDefault
    <> value defaultPortNumber
    )
  <*> option (maybeReader parseURI)
    (
       long "jwk"
    <> help "Path or URI to retrieve JOSE Web Key Set to authenticate tokens against."
    <> showDefault
    <> value defaultJWKSURI
    )
  <*> strOption
    (
       long "aud"
    <> short 'a'
    <> help "OAuth audience claim to match against user tokens."
    )
  )

parsedServerOptions :: Parser ServerOptionsOR
parsedServerOptions = ServerOptionsOR
  <$> strOption
    (
         long "conn"
    <>  short 'c'
    <>  help "Database connection string in libpq format. See: https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
    <>  showDefault
    <>  value defaultDatabaseConnectionString
    )
  <*> option auto
    (  long "log-level"
    <> value InfoS
    <> showDefault
    <> help ("Logging level: " ++ show [minBound .. maxBound :: Severity])
    )
  <*> optional (strOption
    (  long "log-path"
    <> short 'l'
    <> help "Path to write log output to (defaults to stdout)"
    ) )
  <*> option auto
    ( long "env" <> short 'e'
    <> value Dev <> showDefault
    <> help "Environment, Dev | Prod"
    )


-- TODO: Add flag to change from interactive confirmation to instead be automatic operation (so this command can be used from scripts or whatnot) (e.g. runIfSafe) .
initDb :: Parser ExecMode
initDb = pure InitDb


userCommand :: Parser ExecMode
userCommand = UserAction <$> userCommands


userCommands :: Parser UserCommand
userCommands = subparser
  ( mconcat
    [ standardCommand "add"  userAdd  "Add a new user to the registry"
    , standardCommand "list" userList "List all user and their Ids"
    ]
  )


userAdd :: Parser UserCommand
userAdd = pure UserAdd


-- | List all fo the users. (notionally this is listUser, but we use the name
-- userList to perserve the command action format in fucntion names) .
userList :: Parser UserCommand
userList = pure UserList


orgCommand :: Parser ExecMode
orgCommand = OrgAction <$> orgCommands


orgCommands :: Parser OrgCommand
orgCommands = subparser
  ( mconcat
    [ standardCommand "add"  orgAdd  "Add a new org to the registry"
    , standardCommand "list" orgList "List all orgs and their Ids"
    ]
  )


orgAdd :: Parser OrgCommand
orgAdd = pure OrgAdd


-- | List all fo the users. (notionally this is listUser, but we use the name
-- userList to perserve the command action format in fucntion names) .
orgList :: Parser OrgCommand
orgList = pure OrgList


bootstrap :: Parser ExecMode
bootstrap = Bootstrap <$> oAuthSubSuffixParser <*> companyPrefixParser


populateDb :: Parser ExecMode
populateDb = pure PopulateDatabase


oAuthSubSuffixParser :: Parser Text
oAuthSubSuffixParser = argument str (metavar "oAuth Sub Suffix")


emailReader :: ReadM EmailAddress
emailReader = eitherReader (A.parseOnly addrSpec . BS.pack)


companyPrefixParser :: Parser GS1CompanyPrefix
companyPrefixParser = GS1CompanyPrefix <$> (argument str (metavar "GS1CompanyPrefix"))
