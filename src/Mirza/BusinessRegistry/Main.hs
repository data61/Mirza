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
import           Mirza.BusinessRegistry.Service
import           Mirza.BusinessRegistry.Types            as BT
import           Mirza.Common.Types                      as CT
import           Mirza.Common.Utils                      (randomText)

import           Data.GS1.EPC                            (GS1CompanyPrefix (..))

import           Servant                                 hiding (header)
import           Servant.Swagger.UI

import qualified Data.Pool                               as Pool
import           Database.PostgreSQL.Simple

import           Network.Wai                             (Middleware)
import qualified Network.Wai.Handler.Warp                as Warp

import           Data.ByteString                         (ByteString)
import           Data.Semigroup                          ((<>))
import           Data.Text                               (Text, pack)
import           Data.Text.Encoding                      (encodeUtf8)
import           Options.Applicative                     hiding (action)
import           Text.Email.Validate

import qualified Crypto.Scrypt                           as Scrypt

import           Control.Exception                       (finally)
import           Data.Maybe                              (fromMaybe)
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

data ServerOptionsBR = ServerOptionsBR
  { sobDbConnStr    :: ByteString
  , sobScryptN      :: Integer
  , sobScryptP      :: Integer
  , sobScryptR      :: Integer
  , sobLoggingLevel :: K.Severity
  , sobLogLocation  :: Maybe FilePath
  , sobEnvType      :: CT.EnvType
  }

data RunServerOptions = RunServerOptions
  { rsoPortNumber :: Int
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
  RunServer rsOpts  -> launchServer opts rsOpts
  InitDb            -> runMigration opts
  UserAction uc     -> runUserCommand opts uc
  BusinessAction bc -> runBusinessCommand opts bc
  PopulateDatabase  -> runPopulateDatabase opts


--------------------------------------------------------------------------------
-- Service
--------------------------------------------------------------------------------

launchServer :: ServerOptionsBR -> RunServerOptions -> IO ()
launchServer opts rso = do
      let portNumber = rsoPortNumber rso
      ctx <- initBRContext opts
      app <- initApplication opts rso ctx
      mids <- initMiddleware opts rso
      putStrLn $ "http://localhost:" ++ show portNumber ++ "/swagger-ui/"
      Warp.run (fromIntegral portNumber) (mids app) `finally` closeScribes (BT._brKatipLogEnv ctx)


initBRContext :: ServerOptionsBR -> IO BT.BRContext
initBRContext opts@(ServerOptionsBR dbConnStr _ _ _ lev mlogPath envT) = do
  logHandle <- maybe (pure stdout) (flip openFile AppendMode) mlogPath
  hPutStr stderr $ "(Logging will be to: " ++ fromMaybe "stdout" mlogPath ++ ") "
  handleScribe <- mkHandleScribe ColorIfTerminal logHandle lev V3
  logEnv <- initLogEnv "businessRegistry" (Environment . pack . show $ envT)
            >>= registerScribe "stdout" handleScribe defaultScribeSettings
  params <- createScryptParams opts
  connpool <- Pool.createPool (connectPostgreSQL dbConnStr) close
                      1 -- Number of "sub-pools",
                      60 -- How long in seconds to keep a connection open for reuse
                      10 -- Max number of connections to have open at any one time
                      -- TODO: Make this a config paramete
  pure $ BRContext envT connpool params logEnv mempty mempty


initApplication :: ServerOptionsBR -> RunServerOptions -> BT.BRContext -> IO Application
initApplication _go _so ev =
  pure $ serveWithContext api
          (basicAuthServerContext ev)
          (server ev)


initMiddleware :: ServerOptionsBR -> RunServerOptions -> IO Middleware
initMiddleware _ _ = pure id

-- Implementation
server :: BRContext -> Server API
server ev =
  swaggerSchemaUIServer serveSwaggerAPI
  :<|> hoistServerWithContext
        (Proxy @ServerAPI)
        (Proxy @'[BasicAuthCheck BT.AuthUser])
        (appMToHandler ev)
        (appHandlers @BRContext @BRError)


--------------------------------------------------------------------------------
-- Migration Command
--------------------------------------------------------------------------------

runMigration :: ServerOptionsBR -> IO ()
runMigration opts = do
  ctx <- initBRContext opts
  res <- runMigrationWithConfirmation @BRContext @SqlError ctx interactiveMigrationConfirm
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
  newUserEmailAddress <- getUserEmailInteractive
  newUserPassword     <- pack <$> prompt "Password:"
  newUserCompany      <- GS1CompanyPrefix . read <$> prompt "GS1CompanyPrefix:"
  newUserFirstName    <- pack <$> prompt "First Name:"
  newUserLastName     <- pack <$> prompt "Last Name:"
  newUserPhoneNumber  <- pack <$> prompt "Phone Number:"
  pure NewUser{..}

getUserEmailInteractive :: IO EmailAddress
getUserEmailInteractive = do
  userEmail <- encodeUtf8 . pack <$> prompt "Email Address"
  case validate userEmail of
    Left reason -> do
      putStrLn $ "Invalid Email. Reason: " ++ reason
      getUserEmailInteractive
    Right email -> pure email

createScryptParams :: ServerOptionsBR -> IO Scrypt.ScryptParams
createScryptParams ServerOptionsBR{sobScryptN,sobScryptP,sobScryptR} =
  case Scrypt.scryptParams (max sobScryptN 14) (max sobScryptP 8) (max sobScryptR 1) of
    Just scparams -> pure scparams
    Nothing -> do
      putStrLn $  "Invalid Scrypt params:" ++ show (sobScryptN,sobScryptP,sobScryptR)
               ++ " using defaults"
      pure Scrypt.defaultParams


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
  biz_gs1_company_prefix <- GS1CompanyPrefix . pack <$>  prompt "GS1CompanyPrefix"
  biz_name      <- pack <$> prompt "Name"
  let biz_last_update = Nothing
  pure BusinessT{..}

prompt :: String -> IO String
prompt message = putStrLn message *> getLine


--------------------------------------------------------------------------------
-- Populate Database Command : TODO: This is for testing and debugging only and should be removed.
--------------------------------------------------------------------------------

runPopulateDatabase :: ServerOptionsBR -> IO ()
runPopulateDatabase opts = do
  ctx     <- initBRContext opts

  b1      <- dummyBusiness "1"
  _result <- runAppM @_ @BRError ctx $ addBusiness b1
  u1b1    <- dummyUser "B1U1" (newBusinessGS1CompanyPrefix b1)
  u2b1    <- dummyUser "B1U2" (newBusinessGS1CompanyPrefix b1)
  _result <- runAppM @_ @BRError ctx $
             runDb (mapM addUserQuery [u1b1, u2b1])

  b2      <- dummyBusiness "2"
  _result <- runAppM @_ @BRError ctx $ addBusiness b2
  u1b2    <- dummyUser "B2U1" (newBusinessGS1CompanyPrefix b2)
  u2b2    <- dummyUser "B2U2" (newBusinessGS1CompanyPrefix b2)
  _result <- runAppM @_ @BRError ctx $
             runDb (mapM addUserQuery [u1b2, u2b2])

  putStrLn "Credentials"
  printCredentials u1b1
  printCredentials u2b1
  printCredentials u1b2
  printCredentials u2b2

  putStrLn "Full User Information"
  print b1
  print u1b1
  print u2b1

  print b2
  print u1b2
  print u2b2


dummyBusiness :: Text -> IO NewBusiness
dummyBusiness unique = do
  let newBusinessGS1CompanyPrefix = GS1CompanyPrefix ("Business" <> unique <> "Prefix")
  let newBusinessName             = "Business" <> unique <> "Name"
  pure NewBusiness{..}


dummyUser :: Text -> GS1CompanyPrefix -> IO NewUser
dummyUser unique business_uid = do
  passwordEntropy <- randomText
  let newUserEmailAddress = unsafeEmailAddress (encodeUtf8 unique) "example.com"
  let newUserPassword     = "User" <> unique <> "Password" <> passwordEntropy
  let newUserCompany      = business_uid
  let newUserFirstName    = "User" <> unique <> "FirstName"
  let newUserLastName     = "User" <> unique <> "LastName"
  let newUserPhoneNumber  = "User" <> unique <> "PhoneNumber"
  pure NewUser{..}


printCredentials :: NewUser -> IO ()
printCredentials user = do
  putStrLn $ "Username: " <> show (newUserEmailAddress user)
  putStrLn $ "Password: " <> show (newUserPassword user)


--------------------------------------------------------------------------------
-- Debug Command
--------------------------------------------------------------------------------

-- This is a debug function for activating development test stub functions.
-- TODO: Remove this stub before release.
debugFunc :: IO()
debugFunc = do
  putStrLn "Running Debug Option"
  -- Debug test code goes here...


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
          [ standardCommand "server"   runServer "Run HTTP server"
          , standardCommand "initdb"   initDb "Initialise the Database (Note: This command only works if the database \
                                              \is empty and can't be used for migrations or if the database already \
                                              \contains the schema."
          , standardCommand "user"     userCommand "Interactively add new users"
          , standardCommand "business" businessCommand "Operations on businesses"
          , standardCommand "populate" populateDb "Populate the database with dummy test data"
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
      (
          long "scryptN"
      <>  help "Scrypt N parameter (>= 14)"
      <>  showDefault
      <>  value 14
      )
  <*> option auto
      (
          long "scryptP"
      <>  help "Scrypt r parameter (>= 8)"
      <>  showDefault
      <>  value 8
      )
  <*> option auto
      (  long "scryptR"
      <> help "Scrypt r parameter (>= 1)"
      <> showDefault
      <> value 1
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

populateDb :: Parser ExecMode
populateDb = pure PopulateDatabase

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
