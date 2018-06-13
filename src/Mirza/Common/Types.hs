{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}



module Mirza.Common.Types
  ( EnvType(..)
  , AppM(..)
  , runAppM
  , DB(..)
  , runDb
  , pg
  , AsSqlError(..)
  , HasConnPool(..)
  , HasEnvType(..)
  , HasScryptParams(..)
  , HasKatipContext(..)
  , HasKatipLogEnv(..)
  , DBConstraint
  , ask
  , asks
  , throwing
  , throwing_
  ) where


import qualified Database.Beam              as B
import           Database.Beam.Postgres     (Pg)
import           Database.PostgreSQL.Simple (Connection, SqlError)
import qualified Database.PostgreSQL.Simple as DB

import qualified Control.Exception          as Exc
import           Control.Monad.Except       (ExceptT (..), MonadError,
                                             runExceptT, throwError)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, ask, asks,
                                             liftIO, local, runReaderT)
import           Control.Monad.Trans        (lift)

import qualified Control.Exception          as E

import           Data.Pool                  as Pool

import           Crypto.Scrypt              (ScryptParams)


import           Control.Lens
import           Control.Monad.Error.Lens

import           Katip                      as K
import           Katip.Monadic              (askLoggerIO)


data EnvType = Prod | Dev
  deriving (Show, Eq, Read)

-- | The class of contexts which include an 'EnvType'
$(makeClassy ''EnvType)


-- runReaderT :: r -> m a
-- ReaderT r m a
-- type Handler a = ExceptT ServantErr IO a
-- newtype ExceptT e m a :: * -> (* -> *) -> * -> *
newtype AppM context err a = AppM
  { unAppM :: ReaderT context (ExceptT err IO) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader context
    , MonadIO
    , MonadError err
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


-- | The class of contexts which have a database pool:
-- @
--  pool <- view connPool
--  Pool.withResource pool $ \conn -> ..
-- @
class HasConnPool a where
    connPool :: Lens' a (Pool Connection)

-- | The class of error types which can contain a `SqlError`. _See
-- 'Mirza.SupplyChain.BeamQueries.insertUser' for a good example of how to catch
-- errors using this class._
class AsSqlError a where
  _SqlError :: Prism' a SqlError

-- | The class of contexts which have Scrypt parameters
class HasScryptParams a where
  scryptParams :: Lens' a ScryptParams

class HasKatipLogEnv a where
  katipLogEnv :: Lens' a K.LogEnv

class HasKatipContext a where
  katipContexts :: Lens' a K.LogContexts
  katipNamespace :: Lens' a K.Namespace


instance HasKatipLogEnv context => Katip (AppM context err) where
  getLogEnv = view katipLogEnv
  localLogEnv f = local (over katipLogEnv f)

instance (HasKatipContext context, HasKatipLogEnv context)
    => KatipContext (AppM context err) where
  getKatipContext = view katipContexts
  getKatipNamespace = view katipNamespace
  localKatipContext f = local (over katipContexts f)
  localKatipNamespace f = local (over katipNamespace f)

instance HasKatipLogEnv context => Katip (DB context err) where
  getLogEnv = view (_2 . katipLogEnv)
  localLogEnv f = local (over (_2 . katipLogEnv) f)


instance (HasKatipContext context, HasKatipLogEnv context)
    => KatipContext (DB context err) where
  getKatipContext = view (_2 . katipContexts)
  getKatipNamespace = view (_2 . katipNamespace)
  localKatipContext f = local (over (_2 . katipContexts) f)
  localKatipNamespace f = local (over (_2 . katipNamespace) f)


type DBConstraint context err =
    ( HasEnvType context
    , HasConnPool context
    , HasKatipContext context
    , HasKatipLogEnv context
    , AsSqlError err)

-- | Run a DB action within a transaction. See the documentation for
-- 'withTransaction'. SqlError exceptions will be caught and lifted into the
-- AppM MonadError instance, as will all app errors thrown in the DB a action,
-- and in either case the database transaction is rolled back.
--
-- Exceptions which are thrown which are not SqlErrors will be caught by Servant
-- and cause 500 errors (these are not exceptions we'll generally know how to
-- deal with).
runDb ::  DBConstraint context err => DB context err a -> AppM context err a
runDb (DB act) = katipAddNamespace "runDb" $ do
  env <- ask
  e <- view envType
  lggr <- askLoggerIO
  let dbf =  case e of
            Prod -> B.withDatabase
            _    -> B.withDatabaseDebug (lggr DebugS . logStr)

  res <- liftIO $ Pool.withResource (env ^. connPool) $ \conn ->
          Exc.try
         . withTransaction conn
         . dbf conn
         . runExceptT
         . runReaderT act $ (conn,env)
        -- :: AppM (Either SqlError (Either AppError a))
  either (throwing _SqlError)
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


pg :: Pg a -> DB context err a
pg = DB . lift . lift

runAppM :: context -> AppM context err a -> IO (Either err a)
runAppM env aM = runExceptT $ (runReaderT . unAppM) aM env
