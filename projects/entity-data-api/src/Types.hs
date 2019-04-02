{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where


import qualified Control.Exception      as Exc
import           Control.Monad.Except   (ExceptT (..), MonadError, runExceptT,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, asks, local,
                                         runReaderT)

-- runReaderT :: r -> m a
-- ReaderT r m a
-- type Handler a = ExceptT ServantErr IO a
-- newtype ExceptT e m a :: * -> (* -> *) -> * -> *
newtype AppM context err a = AppM
  { getAppM :: ReaderT context (ExceptT err IO) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader context
    , MonadIO
    , MonadError err
    )

runAppM :: context -> AppM context err a -> IO (Either err a)
runAppM env aM = runExceptT $ (runReaderT . getAppM) aM env

