{-# LANGUAGE OverloadedStrings      #-}

module Utils where

import           AppConfig (AppM(..), getEnvType, EnvType(..))
import           Control.Monad (when)
import           Control.Monad.Reader (liftIO)
import           Data.String (IsString)

debugLog :: (IsString a, Show a) => a -> AppM ()
debugLog str = do
  envT <- getEnvType
  when (envT == Dev) $ liftIO $ putStrLn $ show str
