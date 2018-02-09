
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module AppConfig where

import           Control.Exception.Lifted (try)
import           Database.PostgreSQL.Simple (Connection, SqlError(..))
import qualified Database.Beam as B
import           Database.Beam.Postgres (Pg)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT,
                                         asks, liftIO)
import           Control.Monad.Except (MonadError, ExceptT(..), runExceptT)
import qualified Model as M
import qualified Control.Exception as Exc
import           Errors (ServiceError(..))

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

data AppError = AppError ServiceError

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
