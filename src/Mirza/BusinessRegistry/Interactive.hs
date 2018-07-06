{-# LANGUAGE TypeApplications #-}

module Mirza.BusinessRegistry.Interactive
  (runMigrationInteractive) where


import           Mirza.BusinessRegistry.Database.Schema
import           Mirza.Common.Types

import qualified Data.ByteString.Lazy.Char8             as BSL

import           Control.Monad.IO.Class                 (liftIO)

import           Control.Lens                           (view, _1)

import           Database.Beam.Migrate.Simple           (runSimpleMigration,
                                                         simpleMigration)
import           Database.Beam.Postgres                 (Pg, PgCommandSyntax,
                                                         Postgres)
import           Database.Beam.Postgres.Migrate         (migrationBackend)
import           Database.Beam.Postgres.Syntax          (fromPgCommand,
                                                         pgRenderSyntaxScript)



-- TODO: Use autoMigrate if possible and confirm with the user whether to do dangerous migrations explicitly
-- TODO: Move this into Mirza.Common.Beam
runMigrationInteractive ::
  (HasKatipLogEnv context
  , HasKatipContext context
  , HasConnPool context
  , HasEnvType context
  , AsSqlError err)
  => context -> IO (Either err ())
runMigrationInteractive context =
  runAppM context $ runDb $ do
    conn <- view _1
    liftIO $ do
      mcommands <- simpleMigration migrationBackend conn checkedBusinessRegistryDB
      case mcommands of
        Nothing -> fail "lol" -- TODO: Actually implment error handling here.
        Just [] -> putStrLn "Already up to date"
        Just commands -> do
            mapM_ (BSL.putStrLn . pgRenderSyntaxScript . fromPgCommand) commands
            putStrLn "type YES to confirm applying this migration:"
            confirm <- getLine
            case confirm of
              "YES" ->
                runSimpleMigration
                            @PgCommandSyntax
                            @Postgres
                            @_
                            @Pg
                            conn commands
              _ ->  putStrLn "Nothing done."