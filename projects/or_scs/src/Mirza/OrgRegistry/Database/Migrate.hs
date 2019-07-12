module Mirza.OrgRegistry.Database.Migrate ( migrations
                                          , runMigrationSimple
                                          , dropTablesSimple
                                          ) where

import Mirza.Common.Database
import Database.PostgreSQL.Simple

migrations :: [Migration]
migrations = [ m_0001 ]

m_0001 :: Migration
m_0001 conn = do
  _ <- execute_ conn "CREATE TABLE orgs (org_gs1_company_prefix TEXT PRIMARY KEY, org_name TEXT NOT NULL, org_url TEXT NOT NULL, last_update TIMESTAMP);"
  _ <- execute_ conn "CREATE TABLE location (location_gln TEXT PRIMARY KEY, location_org_id TEXT NOT NULL REFERENCES orgs(org_gs1_company_prefix) ON DELETE CASCADE, last_update TIMESTAMP);"
  _ <- execute_ conn "CREATE TABLE users (oauth_sub TEXT PRIMARY KEY, last_update TIMESTAMP);"
  
  _ <- execute_ conn "CREATE TABLE geo_location (geo_location_id UUID PRIMARY KEY, geo_location_gln TEXT NOT NULL REFERENCES location(location_gln) ON DELETE CASCADE, geo_location_lat DOUBLE PRECISION, geo_location_lon DOUBLE PRECISION, geo_location_address TEXT, last_update TIMESTAMP);"
  _ <- execute_ conn "CREATE TABLE org_mapping (mapping_org_id TEXT NOT NULL REFERENCES orgs(org_gs1_company_prefix) ON DELETE CASCADE, mapping_user_oauth_sub TEXT NOT NULL REFERENCES users(oauth_sub) ON DELETE CASCADE, last_update TIMESTAMP, PRIMARY KEY(mapping_org_id, mapping_user_oauth_sub))"
  _ <- execute_ conn "CREATE TABLE keys (key_id UUID PRIMARY KEY, key_org TEXT NOT NULL REFERENCES orgs(org_gs1_company_prefix) ON DELETE CASCADE, jwk JSON NOT NULL, creation_time TIMESTAMP, revocation_time TIMESTAMP, revoking_user_id TEXT REFERENCES users(oauth_sub) ON DELETE CASCADE, expiration_time TIMESTAMP, last_update TIMESTAMP);"

  createTrigger conn "orgs"
  createTrigger conn "location"
  createTrigger conn "users"
  createTrigger conn "geo_location"
  createTrigger conn "org_mapping"
  createTrigger conn "keys"

    

-- --------------------------------------------------------------------------------
-- -- Datatypes
-- --------------------------------------------------------------------------------

-- -- | Datatype to encode whether the migration should be completed or aborted.
-- data Confirmation
--   = Abort
--   | Execute


-- -- Note: Migrations are currently broken, this function can only be used to
-- -- initalise the database from scratch.
-- -- TODO: Use autoMigrate if possible and confirm with the user whether to do
-- --       dangerous migrations explicitly
-- -- TODO: Move this into Mirza.Common.Beam
-- runMigrationWithConfirmation ::  (Member context '[HasLogging, HasDB]
--                                  ,Member err     '[AsSqlError])
--                               => context
--                               -> ([PgCommandSyntax] -> IO Confirmation)
--                               -> IO (Either err ())
-- runMigrationWithConfirmation context confirmationCheck =
--   runAppM context $ runDb $ do
--     conn <- view _1
--     runTriggers <- liftIO $
--       simpleMigration runBeamPostgres migrationBackend conn checkedOrgRegistryDB >>= \case
--         Nothing -> fail "Migration unsuccessful" -- TODO: Actually implment error handling here.
--         Just [] -> False <$ putStrLn "Already up to date"
--         Just commands -> confirmationCheck commands >>= \case
--             Abort -> False <$ putStrLn "Aborting"
--             Execute ->
--               True <$ runSimpleMigration runBeamPostgres conn commands
--     when runTriggers $ addLastUpdateTriggers orgRegistryDB


-- interactiveMigrationConfirm :: [PgCommandSyntax] -> IO Confirmation
-- interactiveMigrationConfirm commands = do
--   mapM_ (BSL.putStrLn . pgRenderSyntaxScript . fromPgCommand) commands
--   putStrLn "type YES to confirm applying this migration:"
--   confirm <- getLine
--   pure $ case confirm of
--     "YES" -> Execute
--     _     -> Abort

