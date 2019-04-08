{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Mirza.EntityDataAPI.Types where

import           System.Envy               (DefConfig (..), FromEnv (..),
                                            Var (..), envMaybe, (.!=))
import qualified System.Envy               as Envy

import           Network.HTTP.ReverseProxy (ProxyDest (..))

import           Control.Monad.Except      (ExceptT (..), MonadError,
                                            runExceptT)
import           Control.Monad.IO.Class    (MonadIO)
import           Control.Monad.Reader      (MonadReader, ReaderT, liftIO,
                                            runReaderT)

import           GHC.Generics              (Generic)

import           Network.HTTP.Client       (Manager)

import           Crypto.JOSE               (JWKSet)

import           Control.Monad.Time        (MonadTime (..))
import           Data.Time.Clock           (getCurrentTime)

import           Text.Read                 (readMaybe)

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
runAppM ctx aM = runExceptT $ (runReaderT . getAppM) aM ctx

data AuthContext = AuthContext
  { myProxyServiceInfo   :: ServiceInfo
  , destProxyServiceInfo :: ProxyDest
  , appManager           :: Manager
  , jwtSigningKeys       :: JWKSet
  } deriving (Generic)


newtype Hostname = Hostname { getHostname :: String } deriving (Eq, Show, Read, Generic)
newtype Port = Port { getPort :: Int } deriving (Eq, Show, Read, Generic)

instance Var Hostname where
  toVar = getHostname
  fromVar = Just . Hostname -- can this be a smart constructor?

instance Var Port where
  toVar = show . getPort
  fromVar s = Port <$> (readMaybe s :: Maybe Int)

data ServiceInfo = ServiceInfo
  { serviceHost :: Hostname
  , servicePort :: Port
  } deriving (Eq, Show, Read, Generic)

fromEnvMyServiceInfo :: Envy.Parser ServiceInfo
fromEnvMyServiceInfo = ServiceInfo
    <$> envMaybe "MY_HOST" .!= Hostname "localhost"
    <*> envMaybe "MY_PORT" .!= Port 8080

fromEnvDestServiceInfo :: Envy.Parser ServiceInfo
fromEnvDestServiceInfo = ServiceInfo
    <$> envMaybe "DEST_HOST" .!= Hostname "localhost"
    <*> envMaybe "DEST_PORT" .!= Port 8000

defaultJwkUrl :: String
defaultJwkUrl = "https://mirza.au.auth0.com/.well-known/jwks.json"

data Opts = Opts
  { myServiceInfo   :: ServiceInfo
  , destServiceInfo :: ServiceInfo
  , jwkUrl          :: String
  } deriving (Show, Generic, Eq)

instance DefConfig Opts where
  defConfig = Opts
    { myServiceInfo   = ServiceInfo{serviceHost=Hostname "localhost", servicePort=Port 8080 }
    , destServiceInfo = ServiceInfo{serviceHost=Hostname "localhost", servicePort=Port 8000 }
    , jwkUrl          = defaultJwkUrl
    }

instance FromEnv Opts where
  fromEnv = Opts
    <$> fromEnvMyServiceInfo
    <*> fromEnvDestServiceInfo
    <*> envMaybe "JWK_URL" .!= defaultJwkUrl
