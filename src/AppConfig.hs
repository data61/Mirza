
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module AppConfig (
  AppM (..)
  , dbFunc
  , runDb
  , mkEnvType
  , Env(..)
  , AppError(..)
)
where

import           Control.Exception.Lifted (try)
import           Database.PostgreSQL.Simple (Connection, SqlError(..))
import qualified Database.Beam as B
import           Database.Beam.Postgres (Pg)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT,
                                         asks, liftIO)
-- import           GHC.Word               (Word16)
import           Servant.Server (Handler, ServantErr)
import           Control.Monad.Except (MonadError, ExceptT(..))
import qualified Model as M
import qualified Control.Exception as Exc

data EnvType = Prod | Dev

mkEnvType :: Bool -> EnvType
mkEnvType False = Prod
mkEnvType _ = Dev

data Env = Env
  { envType :: EnvType
  , dbConn  :: Connection
  -- , port    :: Word16
  }

data AppError = DBErr M.DBError
              | SigErr M.SigError
              | GetPropErr M.GetPropertyError

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
    Prod -> pure $ B.withDatabase conn
    _    -> pure $ B.withDatabaseDebug putStrLn conn

-- | Helper function to run db functions
-- runDb :: Pg b -> AppM b
runDb :: Pg a -> AppM (Either SqlError a)
runDb q = do
  -- df <- dbFunc
  dbFunc >>= (\f -> liftIO $ Exc.try $ f q)

