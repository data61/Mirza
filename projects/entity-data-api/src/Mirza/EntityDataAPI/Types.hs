{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mirza.EntityDataAPI.Types where

import           Network.HTTP.ReverseProxy (ProxyDest (..))

import           Control.Monad.Except      (ExceptT (..), MonadError,
                                            runExceptT)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, liftIO,
                                            runReaderT)
import           Control.Monad.Time        (MonadTime (..))

import           GHC.Generics              (Generic)

import           Network.HTTP.Client       (Manager)

import           Crypto.JOSE               (JWK)

import           Data.Time.Clock           (getCurrentTime)

type ServiceInfo = (String, Int) -- (Host, Port)

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

instance MonadTime (AppM AuthContext err) where
  currentTime = liftIO getCurrentTime

runAppM :: context -> AppM context err a -> IO (Either err a)
runAppM env aM = runExceptT $ (runReaderT . getAppM) aM env


data AuthContext = AuthContext
  { myProxyServiceInfo   :: ServiceInfo
  , destProxyServiceInfo :: ProxyDest
  , appManager           :: Manager
  , jwtSigningKey        :: JWK
  } deriving (Generic)
