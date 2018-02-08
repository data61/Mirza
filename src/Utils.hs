{-# LANGUAGE OverloadedStrings      #-}

module Utils where

import           AppConfig (AppM(..), getEnvType, EnvType(..))
import           Control.Monad (when)
import           Control.Monad.Reader (liftIO, MonadIO)

-- | Given a stringLike, prints it only if the application is in Dev mode
-- otherwise, does a nop.
-- Only works in AppM monad
debugLog :: Show a => a -> AppM ()
debugLog strLike = do
  envT <- getEnvType
  when (envT == Dev) $ liftIO $ putStrLn $ show strLike

-- | To be used when the Env is known/available.
-- It doesn't require that the function is being run in AppM
debugLogGeneral :: (Show a, MonadIO f) => EnvType -> a -> f ()
debugLogGeneral envT strLike = do
  when (envT == Dev) $ liftIO $ putStrLn$ show strLike

