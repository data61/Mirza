{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

-- This module runs database migrations for our application.

module Mirza.BusinessRegistry.Database.Migrate
  ( interactiveMigrationConfirm
  , runMigrationWithConfirmation
  , Confirmation(..)
  ) where

import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.Common.Types
import           Mirza.Common.Utils                     (addLastUpdateTriggers)

import qualified Data.ByteString.Lazy.Char8             as BSL

import           Control.Monad                          (when)
import           Control.Monad.IO.Class                 (liftIO)

import           Control.Lens                           (view, _1)

import           Database.Beam.Migrate.Simple           (runSimpleMigration,
                                                         simpleMigration)
import           Database.Beam.Postgres                 (Pg, PgCommandSyntax,
                                                         Postgres)
import           Database.Beam.Postgres.Migrate         (migrationBackend)
import           Database.Beam.Postgres.Syntax          (fromPgCommand,
                                                         pgRenderSyntaxScript)


--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- | Datatype to encode whether the migration should be completed or aborted.
data Confirmation
  = Abort
  | Execute



-- Note: Migrations are currently broken, this function can only be used to
-- initalise the database from scratch.
-- TODO: Use autoMigrate if possible and confirm with the user whether to do
--       dangerous migrations explicitly
-- TODO: Move this into Mirza.Common.Beam
runMigrationWithConfirmation :: -- (Member context '[HasLogging, HasDB]
                                -- ,Member err     '[AsSqlError])
  (HasKatipLogEnv context
  , HasKatipContext context
  , HasConnPool context
  , HasEnvType context
  , AsSqlError err)
  => context -> ([PgCommandSyntax] -> IO Confirmation) -> IO (Either err ())
runMigrationWithConfirmation context confirmationCheck =
  runAppM context $ runDb $ do
    conn <- view _1
    runTriggers <- liftIO $
      simpleMigration migrationBackend conn checkedBusinessRegistryDB >>= \case
        Nothing -> fail "lol" -- TODO: Actually implment error handling here.
        Just [] -> False <$ putStrLn "Already up to date"
        Just commands -> confirmationCheck commands >>= \case
            Abort -> pure False
            Execute ->
              True <$ runSimpleMigration @PgCommandSyntax @Postgres @_ @Pg
                        conn commands
    when runTriggers $ addLastUpdateTriggers businessRegistryDB


interactiveMigrationConfirm :: [PgCommandSyntax] -> IO Confirmation
interactiveMigrationConfirm commands = do
  mapM_ (BSL.putStrLn . pgRenderSyntaxScript . fromPgCommand) commands
  putStrLn "type YES to confirm applying this migration:"
  confirm <- getLine
  pure $ case confirm of
    "YES" -> Execute
    _     -> Abort

