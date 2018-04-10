
-- | General utility functions used throughout the codebase
module Utils where

import           AppConfig            (AppM, EnvType (..), asks, envType)
import           Control.Monad        (when)
import           Control.Monad.Reader (MonadIO, liftIO)
import qualified Data.Text            as T

-- | Given a stringLike, prints it only if the application is in Dev mode
-- otherwise, does a nop.
-- Only works in AppM monad
debugLog :: Show a => a -> AppM ()
debugLog strLike = do
  envT <- asks envType
  when (envT == Dev) $ liftIO $ putStrLn $ show strLike

-- | To be used when the Env is known/available.
-- It doesn't require that the function is being run in AppM
debugLogGeneral :: (Show a, MonadIO f) => EnvType -> a -> f ()
debugLogGeneral envT strLike = do
  when (envT == Dev) $ liftIO $ putStrLn$ show strLike

bun :: String
bun = "========================"

sandwichLog :: Show a => a -> AppM ()
sandwichLog patty = do
  debugLog bun
  debugLog patty
  debugLog bun

toText :: Show a => a -> T.Text
toText = T.pack . show
