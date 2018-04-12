
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | Contains the definition of our ReaderT AppM
module AppConfig where

import qualified Control.Exception          as Exc
import           Control.Monad              (when)
import           Control.Monad.Except       (ExceptT (..), MonadError,
                                             runExceptT)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, asks, liftIO,
                                             runReaderT)
import qualified Database.Beam              as B
import           Database.Beam.Postgres     (Pg)
import           Database.PostgreSQL.Simple (Connection, SqlError (..))
import           Errors                     (ServiceError (..))

data EnvType = Prod | Dev
  deriving (Show, Eq, Read)

mkEnvType :: Bool -> EnvType
mkEnvType False = Prod
mkEnvType _     = Dev

data Env = Env
  { envType :: EnvType
  , dbConn  :: Connection
  -- , port    :: Word16
  }

data AppError = AppError ServiceError deriving (Show, Read)

-- runReaderT :: r -> m a
-- ReaderT r m a
-- type Handler a = ExceptT ServantErr IO a
-- newtype ExceptT e m a :: * -> (* -> *) -> * -> *
newtype AppM a = AppM
  { unAppM :: ReaderT Env (ExceptT AppError IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadIO
           , MonadError AppError
           )

getDBConn :: AppM Connection
getDBConn = asks dbConn

getEnvType :: AppM EnvType
getEnvType = asks envType

dbFunc :: AppM (Pg a -> IO a)
dbFunc = do
  conn <- getDBConn
  e <- getEnvType
  case e of
    Prod -> pure $ B.withDatabase conn  -- database queries other than migration will be silent
    _    -> pure $ B.withDatabaseDebug putStrLn conn  -- database queries other than migration will print on screen

-- | Helper function to run db functions
runDb :: Pg a -> AppM (Either SqlError a)
runDb q = dbFunc >>= (\f -> liftIO $ Exc.try $ f q)


runAppM :: Env -> AppM a -> IO (Either AppError a)
runAppM env aM = runExceptT $ (runReaderT . unAppM) aM env

-- App Utils. Moved from Utils

-- | Given a stringLike, prints it only if the application is in Dev
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
  when (envT == Dev) $ liftIO $ putStrLn $ show strLike

bun :: String
bun = "========================"

sandwichLog :: Show a => a -> AppM ()
sandwichLog patty = do
  debugLog bun
  debugLog patty
  debugLog bun
