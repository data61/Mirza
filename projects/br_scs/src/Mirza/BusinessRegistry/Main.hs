{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Mirza.BusinessRegistry.Main where


import           Mirza.BusinessRegistry.API              (API, ServerAPI, api)
import           Mirza.BusinessRegistry.Auth
import           Mirza.BusinessRegistry.Database.Migrate
import           Mirza.BusinessRegistry.Database.Schema  as Schema
import           Mirza.BusinessRegistry.GenerateUtils    (dummyBusiness,
                                                          dummyUser)
import           Mirza.BusinessRegistry.Service
import           Mirza.BusinessRegistry.Types            as BT
import           Mirza.Common.Types                      as CT

import           Data.GS1.EPC                            (GS1CompanyPrefix (..))

import           Servant
import           Servant.Auth.Server
import           Servant.Swagger.UI

import           Crypto.JWT                              (Audience (..), string)

import qualified Data.Pool                               as Pool
import           Database.PostgreSQL.Simple

import           Network.URI                             (nullURI)
import           Network.Wai                             (Middleware)
import qualified Network.Wai.Handler.Warp                as Warp

import           Data.Aeson                              (eitherDecodeFileStrict)

import qualified Data.Attoparsec.ByteString              as A
import           Data.ByteString                         (ByteString)
import qualified Data.ByteString.Char8                   as BS
import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text, pack)
import           Options.Applicative                     hiding (action)
import           Text.Email.Parser                       (addrSpec)

import           Control.Exception                       (finally)
import           Control.Lens                            (review)
import           Data.Maybe                              (fromMaybe)
import           Data.Either                             (fromRight)
import           Katip                                   as K
import           System.IO                               (IOMode (AppendMode),
                                                          hPutStr, openFile,
                                                          stderr, stdout)


--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Port number changed so that BR and SCS can be run at the same time
defaultPortNumber :: Int
defaultPortNumber = 8200

defaultDatabaseConnectionString :: ByteString
defaultDatabaseConnectionString = "dbname=devbusinessregistry"


--------------------------------------------------------------------------------
-- Command Line Options Data Types
--------------------------------------------------------------------------------
data InitOptionsBR = InitOptionsBR
  { soGlobals  :: ServerOptionsBR
  , soExecMode :: ExecMode
  }

data ExecMode
  = RunServer RunServerOptions
  | InitDb
  | UserAction UserCommand
  | BusinessAction BusinessCommand
  | PopulateDatabase  -- TODO: This option should be removed....this is for testing and debugging only.
  | Bootstrap Text GS1CompanyPrefix

data ServerOptionsBR = ServerOptionsBR
  { sobDbConnStr    :: ByteString
  , sobLoggingLevel :: K.Severity
  , sobLogLocation  :: Maybe FilePath
  , sobEnvType      :: CT.EnvType
  }

data RunServerOptions = RunServerOptions
  { rsoPortNumber    :: Int
  , sobOAuthAudience :: Text
  }

data UserCommand
  = UserAdd
  | UserList

data BusinessCommand
  = BusinessAdd
  | BusinessList


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = multiplexInitOptions =<< execParser opts where
  opts = Options.Applicative.info (serverOptions <**> helper)
    (fullDesc
    <> progDesc "Here to meet all your business registry needs"
    <> header "Supply Chain Business Registry Service")


-- Handles the overriding server options (this effectively defines the point
-- where the single binary could be split into multiple binaries.
multiplexInitOptions :: InitOptionsBR -> IO ()
multiplexInitOptions (InitOptionsBR opts mode) = case mode of
  RunServer rsOpts                       -> launchServer opts rsOpts
  InitDb                                 -> runMigration opts
  UserAction uc                          -> runUserCommand opts uc
  BusinessAction bc                      -> runBusinessCommand opts bc
  PopulateDatabase                       -> runPopulateDatabase opts
  Bootstrap oAuthSubSuffix companyPrefix -> runBootstrap opts oAuthSubSuffix companyPrefix


--------------------------------------------------------------------------------
-- Service
--------------------------------------------------------------------------------

launchServer :: ServerOptionsBR -> RunServerOptions -> IO ()
launchServer opts rso = do
      let portNumber = rsoPortNumber rso
      minimalContext <- initBRContext opts
      completeContext <- addServerOptions minimalContext rso
      app <- initApplication completeContext
      mids <- initMiddleware opts rso
      putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/"
      Warp.run (fromIntegral portNumber) (mids app) `finally` closeScribes (BT._brKatipLogEnv completeContext)


addServerOptions :: BRContextMinimal -> RunServerOptions -> IO BRContextComplete
addServerOptions minimalContext (RunServerOptions _port oauthAudience) = addAuthOptions minimalContext oauthAudience


addAuthOptions :: BRContextMinimal -> Text -> IO BRContextComplete
addAuthOptions minimalContext oauthAudience = do
  eitherJwk <- eitherDecodeFileStrict "auth_public_key_2019-04-01.json"
  let makeError errorMessage = error $ "Unable to get the OAuth Public Key. Error was: " <> (show errorMessage)
  let jwk = either makeError id eitherJwk
  let audience = Audience [review string oauthAudience]
  pure $ brContextComplete minimalContext audience jwk


initBRContext :: ServerOptionsBR -> IO BT.BRContextMinimal
initBRContext (ServerOptionsBR dbConnStr lev mlogPath envT) = do
  logHandle <- maybe (pure stdout) (flip openFile AppendMode) mlogPath
  hPutStr stderr $ "(Logging will be to: " ++ fromMaybe "stdout" mlogPath ++ ") "
  handleScribe <- mkHandleScribe ColorIfTerminal logHandle lev V3
  logEnv <- initLogEnv "businessRegistry" (Environment . pack . show $ envT)
            >>= registerScribe "stdout" handleScribe defaultScribeSettings
  connpool <- Pool.createPool (connectPostgreSQL dbConnStr) close
                      1 -- Number of "sub-pools",
                      60 -- How long in seconds to keep a connection open for reuse
                      10 -- Max number of connections to have open at any one time
                      -- TODO: Make this a config paramete
  pure $ brContextMinimal envT connpool logEnv mempty mempty


initApplication :: BRContextComplete -> IO Application
initApplication ev =
  pure $ serveWithContext api
          (tokenServerContext ev)
          (server ev)


initMiddleware :: ServerOptionsBR -> RunServerOptions -> IO Middleware
initMiddleware _ _ = pure id

-- Implementation
server :: BRContextComplete -> Server API
server ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[CookieSettings, JWTSettings])
        (appMToHandler ev)
        (appHandlers @BRContextComplete @BRError)


--------------------------------------------------------------------------------
-- Migration Command
--------------------------------------------------------------------------------

runMigration :: ServerOptionsBR -> IO ()
runMigration opts = do
  ctx <- initBRContext opts
  res <- runMigrationWithConfirmation @BRContextMinimal @SqlError ctx interactiveMigrationConfirm
  print res


--------------------------------------------------------------------------------
-- User Command
--------------------------------------------------------------------------------

runUserCommand :: ServerOptionsBR -> UserCommand -> IO ()
runUserCommand opts UserList = do
   ctx <- initBRContext opts
   euser <- runAppM ctx $ runDb listUsersQuery
   either (print @BRError) (mapM_ print) euser

runUserCommand opts UserAdd = do
  user <- interactivelyGetNewUser
  ctx <- initBRContext opts
  euser <- runAppM ctx $ runDb (addUserQuery user)
  either (print @BRError) print euser


interactivelyGetNewUser :: IO NewUser
interactivelyGetNewUser = do
  newUserOAuthSub     <- pack <$> prompt "OAuthSub:"
  pure NewUser{..}


--------------------------------------------------------------------------------
-- Business Command
--------------------------------------------------------------------------------

runBusinessCommand :: ServerOptionsBR -> BusinessCommand -> IO ()
runBusinessCommand opts BusinessList = do
  ctx <- initBRContext opts
  ebizs <- runAppM ctx $ runDb (searchBusinessesQuery Nothing Nothing Nothing)
  either (print @BRError) (mapM_ print) ebizs

runBusinessCommand opts BusinessAdd = do
  business <- interactivelyGetBusinessT
  ctx <- initBRContext opts
  ebiz <- runAppM ctx $ runDb (addBusinessQuery business)
  either (print @BRError) print ebiz


interactivelyGetBusinessT :: IO Business
interactivelyGetBusinessT = do
  business_gs1_company_prefix <- GS1CompanyPrefix . pack <$>  prompt "GS1CompanyPrefix:"
  business_name               <- pack <$> prompt "Name:"
  business_url                <- pack <$> prompt "Url:"
  let business_last_update = Nothing
  pure BusinessT{..}

prompt :: String -> IO String
prompt message = putStrLn message *> getLine


--------------------------------------------------------------------------------
-- Populate Database Command : TODO: This is for testing and debugging only and should be removed.
--------------------------------------------------------------------------------

runPopulateDatabase :: ServerOptionsBR -> IO ()
runPopulateDatabase opts = do
  context     <- initBRContext opts
  let runWithContext = runAppM @_ @BRError context
  let right = fromRight (error "Error inserting user.")

  let b1u1 = dummyUser "B1U1"
  b1u1Result <- runWithContext $ addUserOnlyId b1u1
  let b1  =  dummyBusiness "1"
  _result <- runWithContext $ addBusiness (right b1u1Result) b1
  let b1u2 = dummyUser "B1U2"
  b1u2Result <- runWithContext $ addUserOnlyId b1u2
  _result <- runWithContext $ addOrganisationMapping (newBusinessGS1CompanyPrefix b1) (right b1u2Result)

  let b2u1 = dummyUser "B2U1"
  b2u1Result <- runWithContext $ addUserOnlyId b2u1
  let b2  =  dummyBusiness "2"
  _result <- runWithContext $ addBusiness (right b2u1Result) b2
  let b2u2 = dummyUser "B2U2"
  b2u2Result <- runWithContext $ addUserOnlyId b2u2
  _result <- runWithContext $ addOrganisationMapping (newBusinessGS1CompanyPrefix b2) (right b2u2Result)

  putStrLn "Inserted Businesses and Users Information"
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
-- and add other users and businesses into the database. The fact that this user
-- doesn't take proper credentials suggests that our user model is wrong
-- (incomplete)... i.e. we need users that aren't associated with businesses,
-- but we need to do much more work here when we deal with permssions in general.
runBootstrap :: ServerOptionsBR -> Text -> GS1CompanyPrefix -> IO ()
runBootstrap opts email companyPrefix = do
  let newUser = bootstrapUser email
  let newBusiness = bootstrapBusiness companyPrefix

  context <- initBRContext opts

  userResult <- runAppM @_ @BRError context $ addUser newUser
  either (print @BRError) print userResult

  -- We ignore the business insert result, for now we just try best effort.
  -- Can always improve the check and error handling here later if we need to
  -- improve the reliability or error reporting.
  case userResult of
      Right user -> do
                    businessResult <- runAppM @_ @BRError context $ addBusiness (CT.UserId $ user_id user) newBusiness
                    either (print @BRError) print businessResult
      Left _     -> putStrLn "Error inserting user, skipping adding organisation."

  where
    bootstrapUser :: Text -> NewUser
    bootstrapUser oAuthSubSuffix = do
      let newUserOAuthSub     = "bootstrapped-user-oauth-sub" <> oAuthSubSuffix
      NewUser{..}

    bootstrapBusiness :: GS1CompanyPrefix -> NewBusiness
    bootstrapBusiness prefix = do
      let newBusinessGS1CompanyPrefix = prefix
      let newBusinessName             = "Bootstrapped Business"
      let newBusinessUrl              = nullURI
      NewBusiness{..}

--------------------------------------------------------------------------------
-- Command Line Options Argument Parsers
--------------------------------------------------------------------------------

standardCommand :: String -> Parser a -> String -> Mod CommandFields a
standardCommand name action desciption =
  command name (info (action <**> helper) (progDesc desciption))


-- The standard format of the main command line options is [Command] [Action], this applies to things like business and user.
serverOptions :: Parser InitOptionsBR
serverOptions = InitOptionsBR
  <$> parsedServerOptions
  <*> subparser
        ( mconcat
          [ standardCommand "server"    runServer "Run HTTP server"
          , standardCommand "initdb"    initDb "Initialise the Database (Note: This command only works if the database \
                                               \is empty and can't be used for migrations or if the database already \
                                               \contains the schema."
          , standardCommand "user"      userCommand "Interactively add new users"
          , standardCommand "business"  businessCommand "Operations on businesses"
          , standardCommand "populate"  populateDb "Populate the database with dummy test data"
          , standardCommand "bootstrap" bootstrap "Bootstrap a user into the database."
          ]
        )


runServer :: Parser ExecMode
runServer = RunServer <$>
  (RunServerOptions
  <$> option auto
      (
          long "port"
      <>  help "Port to run the service on."
      <>  showDefault
      <>  value defaultPortNumber
      )
  <*> strOption
    (
       long "aud"
    <> short 'a'
    <> help "OAuth audience claim to match against user tokens."
    )
  )

parsedServerOptions :: Parser ServerOptionsBR
parsedServerOptions = ServerOptionsBR
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
        )
      )
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


businessCommand :: Parser ExecMode
businessCommand = BusinessAction <$> businessCommands


businessCommands :: Parser BusinessCommand
businessCommands = subparser
  ( mconcat
    [ standardCommand "add"  businessAdd  "Add a new business to the registry"
    , standardCommand "list" businessList "List all businesses and their Ids"
    ]
  )


businessAdd :: Parser BusinessCommand
businessAdd = pure BusinessAdd


-- | List all fo the users. (notionally this is listUser, but we use the name
-- userList to perserve the command action format in fucntion names) .
businessList :: Parser BusinessCommand
businessList = pure BusinessList


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
