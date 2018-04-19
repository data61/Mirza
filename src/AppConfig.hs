{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Contains the definition of our ReaderT AppM
module AppConfig
  (EnvType(..)
  , mkEnvType
  , Env(..)
  , AppError(..)
  , AppM
  , DB
  , runAppM
  , dbFunc
  , runDb
  , pg
  , ask
  , asks
  , debugLog
  , debugLogGeneral
  , sandwichLog
  )
  where

import qualified Database.Beam              as B
import           Database.Beam.Postgres     (Pg)
import           Database.PostgreSQL.Simple (Connection, withTransaction)

import qualified Control.Exception          as Exc
import           Control.Monad              (when)
import           Control.Monad.Except       (ExceptT (..), MonadError,
                                             runExceptT, throwError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask, asks,
                                             liftIO, runReaderT)
import           Control.Monad.Trans        (lift)
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

newtype AppError = AppError ServiceError deriving (Show)

-- runReaderT :: r -> m a
-- ReaderT r m a
-- type Handler a = ExceptT ServantErr IO a
-- newtype ExceptT e m a :: * -> (* -> *) -> * -> *
newtype AppM a = AppM
  { unAppM :: ReaderT Env (ExceptT AppError IO) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Env
    , MonadIO
    , MonadError AppError
    )


newtype DB a = DB (ReaderT (Connection,Env) (ExceptT AppError Pg) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader (Connection,Env)
  , MonadError AppError
  , MonadIO -- Need to figure out if we actually want this
  )



dbFunc :: Connection -> AppM (Pg a -> IO a)
dbFunc conn = do
  e <- asks envType
  pure $ case e of
    Prod -> B.withDatabase conn  -- database queries other than migration will be silent
    _    -> B.withDatabaseDebug putStrLn conn  -- database queries other than migration will print on screen


-- | Run a DB action within a transaction. See the documentation for
-- 'withTransaction'.
runDb :: DB a -> AppM a
runDb (DB act) = do
  env <- ask
  dbf <- dbFunc (dbConn env)
  res <- liftIO . Exc.try
         . withTransaction (dbConn env)
         . dbf . runExceptT
         $ runReaderT act (dbConn env,env)
        -- :: AppM (Either SqlError (Either AppError a))
  either (throwError . AppError . DatabaseError)
         (either throwError pure)
         res

-- TODO: Remove this once beam version is updated
pg :: Pg a -> DB a
pg = DB . lift . lift

runAppM :: Env -> AppM a -> IO (Either AppError a)
runAppM env aM = runExceptT $ (runReaderT . unAppM) aM env

-- App Utils. Moved from Utils

-- | Given a stringLike, prints it only if the application is in Dev
-- otherwise, does a nop.
-- Only works in AppM monad
debugLog :: Show a => a -> AppM ()
debugLog strLike = do
  envT <- asks envType
  when (envT == Dev) $ liftIO $ print strLike

-- | To be used when the Env is known/available.
-- It doesn't require that the function is being run in AppM
debugLogGeneral :: (Show a, MonadIO f) => EnvType -> a -> f ()
debugLogGeneral envT strLike =
  when (envT == Dev) $ liftIO $ print strLike

bun :: String
bun = "========================"

sandwichLog :: Show a => a -> AppM ()
sandwichLog patty = do
  debugLog bun
  debugLog patty
  debugLog bun
