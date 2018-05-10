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
                                             liftIO, runReaderT)
import           Control.Monad.Trans        (lift)

import qualified Control.Exception          as E

import           Data.Pool                  as Pool

import           Control.Lens
import           Control.Monad.Error.Lens


data EnvType = Prod | Dev
  deriving (Show, Eq, Read)

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



class HasConnPool a where
    connPool :: Lens' a (Pool Connection)

class AsSqlError a where
  _SqlError :: Prism' a SqlError

-- | Run a DB action within a transaction. See the documentation for
-- 'withTransaction'. SqlError exceptions will be caught and lifted into the
-- AppM MonadError instance, as will all app errors thrown in the DB a action,
-- and in either case the database transaction is rolled back.
--
-- Exceptions which are thrown which are not SqlErrors will be caught by Servant
-- and cause 500 errors (these are not exceptions we'll generally know how to
-- deal with).
runDb :: (HasEnvType context,HasConnPool context, AsSqlError err)
      => DB context err a -> AppM context err a
runDb (DB act) = do
  env <- ask
  e <- view envType
  let dbf =  case e of
            Prod -> B.withDatabase
            _    -> B.withDatabaseDebug putStrLn

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
