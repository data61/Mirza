{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Contains the definition of our ReaderT AppM
module Mirza.SupplyChain.AppConfig
  (EnvType(..)
  , mkEnvType
  , SCSContext(..)
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

import           Mirza.SupplyChain.Errors   (ServiceError (..))

import qualified Database.Beam              as B
import           Database.Beam.Postgres     (Pg)
import           Database.PostgreSQL.Simple (Connection)
import qualified Database.PostgreSQL.Simple as DB

import qualified Control.Exception          as Exc
import           Control.Monad              (when)
import           Control.Monad.Except       (ExceptT (..), MonadError,
                                             runExceptT, throwError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask, asks,
                                             liftIO, runReaderT)
import           Control.Monad.Trans        (lift)

import           Crypto.Scrypt              (ScryptParams)

import qualified Control.Exception          as E

import           Data.Pool                  as Pool

data EnvType = Prod | Dev
  deriving (Show, Eq, Read)

mkEnvType :: Bool -> EnvType
mkEnvType False = Prod
mkEnvType _     = Dev

data SCSContext = SCSContext
  { envType    :: EnvType
  , dbConnPool :: Pool Connection
  , scryptPs   :: ScryptParams
  -- , port    :: Word16
  }

newtype AppError = AppError ServiceError deriving (Show)

-- runReaderT :: r -> m a
-- ReaderT r m a
-- type Handler a = ExceptT ServantErr IO a
-- newtype ExceptT e m a :: * -> (* -> *) -> * -> *
newtype AppM a = AppM
  { unAppM :: ReaderT SCSContext (ExceptT AppError IO) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SCSContext
    , MonadIO
    , MonadError AppError
    )

-- | The DB monad is used to connect to the Beam backend. The only way to run
-- something of type DB a is to use 'runDb', which ensures the action is run in
-- a Postgres transaction, and that exceptions and errors thrown inside the DB a
-- cause the transaction to be rolled back and the error rethrown.
newtype DB context error a = DB (ReaderT (Connection,context) (ExceptT error Pg) a)
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadReader (Connection,context)
  , MonadError error
  , MonadIO -- Need to figure out if we actually want this
  )



dbFunc :: AppM (Connection -> Pg a -> IO a)
dbFunc = do
  e <- asks envType
  pure $ case e of
    Prod -> B.withDatabase  -- database queries other than migration will be silent
    _    -> B.withDatabaseDebug putStrLn  -- database queries other than migration will print on screen


-- | Run a DB action within a transaction. See the documentation for
-- 'withTransaction'. SqlError exceptions will be caught and lifted into the
-- AppM MonadError instance, as will all app errors thrown in the DB a action,
-- and in either case the database transaction is rolled back.
--
-- Exceptions which are thrown which are not SqlErrors will be caught by Servant
-- and cause 500 errors (these are not exceptions we'll generally know how to
-- deal with).
runDb :: DB SCSContext AppError a -> AppM a
runDb (DB act) = do
  env <- ask
  dbf <- dbFunc
  res <- liftIO $ Pool.withResource (dbConnPool env) $ \conn ->
          Exc.try
         . withTransaction conn
         . dbf conn
         . runExceptT
         . runReaderT act $ (conn,env)
        -- :: AppM (Either SqlError (Either AppError a))
  either (throwError . AppError . DatabaseError)
         (either throwError pure)
         res


-- | As "Database.PostgreSQL.Simple.Transaction".'DB.withTransaction',
-- but aborts the transaction if a 'Left' is returned.

-- TODO: Add NFData constraint to avoid async exceptions.
withTransaction :: Connection -> IO (Either e a) -> IO (Either e a)
withTransaction conn act = E.mask $ \restore -> do
  DB.begin conn
  r <- restore (act >>= E.evaluate) `E.onException` DB.rollback conn
  case r of
    Left _  -> DB.rollback conn
    Right _ -> DB.commit conn
  pure r


pg :: Pg a -> DB SCSContext AppError a
pg = DB . lift . lift

runAppM :: SCSContext -> AppM a -> IO (Either AppError a)
runAppM env aM = runExceptT $ (runReaderT . unAppM) aM env

-- App Utils. Moved from Utils

-- | Given a stringLike, prints it only if the application is in Dev
-- otherwise, does a nop.
-- Only works in AppM monad
debugLog :: Show a => a -> AppM ()
debugLog strLike = do
  envT <- asks envType
  when (envT == Dev) $ liftIO $ print strLike

-- | To be used when the SCSContext is known/available.
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
