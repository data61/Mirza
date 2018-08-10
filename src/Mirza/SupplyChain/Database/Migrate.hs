{-# LANGUAGE TypeApplications #-}

-- This module runs database migrations for our application.

module Mirza.SupplyChain.Database.Migrate
  ( interactiveMigrationConfirm
  , runMigrationWithConfirmation
  , Confirmation(..)
  ) where

import           Mirza.Common.Types
import           Mirza.SupplyChain.Database.Schema

import qualified Data.ByteString.Lazy.Char8        as BSL

import           Control.Monad.IO.Class            (liftIO)

import           Control.Lens                      (view, _1)

import           Database.Beam.Migrate.Simple      (runSimpleMigration,
                                                    simpleMigration)
import           Database.Beam.Postgres            (Pg, PgCommandSyntax,
                                                    Postgres)
import           Database.Beam.Postgres.Migrate    (migrationBackend)
import           Database.Beam.Postgres.Syntax     (fromPgCommand,
                                                    pgRenderSyntaxScript)



--------------------------------------------------------------------------------
-- Datatypes
--------------------------------------------------------------------------------

-- | Datatype to encode whether the migration should be completed or aborted.
data Confirmation
  = Abort
  | Execute



-- Note: Migrations are currently broken, this function can only be used to initalise the database from scratch.
-- TODO: Use autoMigrate if possible and confirm with the user whether to do dangerous migrations explicitly
-- TODO: Move this into Mirza.Common.Beam
runMigrationWithConfirmation ::
  (HasKatipLogEnv context
  , HasKatipContext context
  , HasConnPool context
  , HasEnvType context
  , AsSqlError err)
  => context -> ([PgCommandSyntax] -> IO Confirmation) -> IO (Either err ())
runMigrationWithConfirmation context confirmationCheck =
  runAppM context $ runDb $ do
    conn <- view _1
    liftIO $ do
      mcommands <- simpleMigration migrationBackend conn checkedSupplyChainDb
      case mcommands of
        Nothing -> fail "lol" -- TODO: Actually implment error handling here.
        Just [] -> putStrLn "Already up to date"
        Just commands -> do
          result <- confirmationCheck commands
          case result of
            Abort ->
              pure ()
            Execute ->
              runSimpleMigration
                      @PgCommandSyntax
                      @Postgres
                      @_
                      @Pg
                      conn commands



interactiveMigrationConfirm :: [PgCommandSyntax] -> IO Confirmation
interactiveMigrationConfirm commands = do
  mapM_ (BSL.putStrLn . pgRenderSyntaxScript . fromPgCommand) commands
  putStrLn "type YES to confirm applying this migration:"
  confirm <- getLine
  case confirm of
    "YES" -> pure Execute
    _     -> pure Abort

