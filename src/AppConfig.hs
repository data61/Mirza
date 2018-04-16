{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Contains the definition of our ReaderT AppM
module AppConfig
  (EnvType(..)
  , mkEnvType
  , Env(..)
  , AppError(..)
  , AppM
  , runAppM
  , dbFunc
  , runDb
  , ask
  , asks
  )
  where

import qualified Database.Beam              as B
import           Database.Beam.Postgres     (Pg)
import           Database.PostgreSQL.Simple (Connection, SqlError)

import qualified Control.Exception          as Exc
import           Control.Monad.Except       (ExceptT (..), MonadError,
                                             runExceptT)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask, asks,
                                             liftIO, runReaderT)
import           Errors                     (ServiceError (..))

import           Crypto.Scrypt              (ScryptParams)

data EnvType = Prod | Dev
  deriving (Show, Eq, Read)

mkEnvType :: Bool -> EnvType
mkEnvType False = Prod
mkEnvType _     = Dev

data Env = Env
  { envType  :: EnvType
  , dbConn   :: Connection
  , scryptPs :: ScryptParams
  -- , port    :: Word16
  }

data AppError = AppError ServiceError deriving (Show)

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


dbFunc :: AppM (Pg a -> IO a)
dbFunc = do
  conn <- asks dbConn
  e <- asks envType
  pure $ case e of
    Prod -> B.withDatabase conn  -- database queries other than migration will be silent
    _    -> B.withDatabaseDebug putStrLn conn  -- database queries other than migration will print on screen

-- | Helper function to run db functions
runDb :: Pg a -> AppM (Either SqlError a)
runDb q = dbFunc >>= (\f -> liftIO $ Exc.try $ f q) -- >>= either (throwError . AppError . DatabaseError) pure
-- TODO: Should this throwError SqlErrors? we have code which ignores the
-- errors, we should require explicit catching.

runAppM :: Env -> AppM a -> IO (Either AppError a)
runAppM env aM = runExceptT $ (runReaderT . unAppM) aM env
