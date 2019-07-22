{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}

module Mirza.EntityDataAPI.Types where

import           System.Envy                (DefConfig (..), FromEnv (..),
                                             Var (..), env, envMaybe, (.!=))
import qualified System.Envy                as Envy

import           Network.HTTP.ReverseProxy  (ProxyDest (..))

import           Control.Monad.Except       (ExceptT (..), MonadError,
                                             runExceptT)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, liftIO,
                                             runReaderT)

import           GHC.Generics               (Generic)

import           Network.HTTP.Client        (Manager)


import           Control.Monad.Time         (MonadTime (..))
import           Data.Time.Clock            (getCurrentTime)

import           Text.Read                  (readMaybe)

import           Crypto.JWT                 (JWKSet, StringOrURI)

import           Data.Pool                  as Pool
import           Database.PostgreSQL.Simple (Connection)

import           Data.ByteString            (ByteString)


--  ------------------- AppM -----------------------
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

instance MonadTime (AppM EDAPIContext err) where
  currentTime = liftIO getCurrentTime

runAppM :: context -> AppM context err a -> IO (Either err a)
runAppM ctx aM = runExceptT $ (runReaderT . getAppM) aM ctx

--  ------------------------------------------------

--  ---------------Context--------------------------

data EDAPIContext = EDAPIContext
  { myProxyServiceInfo     :: ServiceInfo
  , scsProxyServiceInfo    :: ProxyDest
  , trailsProxyServiceInfo :: ProxyDest
  , appManager             :: Manager
  , jwtSigningKeys         :: JWKSet
  , ctxJwkClientIds        :: [StringOrURI]
  , dbConnPool             :: Pool Connection
  } deriving (Generic)

--  ------------------------------------------------


--  ----------------Service Info--------------------

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

fromEnvSCSServiceInfo :: Envy.Parser ServiceInfo
fromEnvSCSServiceInfo = ServiceInfo
    <$> envMaybe "SCS_HOST" .!= Hostname "localhost"
    <*> envMaybe "SCS_PORT" .!= Port 8000

fromEnvTrailsServiceInfo :: Envy.Parser ServiceInfo
fromEnvTrailsServiceInfo = ServiceInfo
    <$> envMaybe "TRAILS_HOST" .!= Hostname "localhost"
    <*> envMaybe "TRAILS_PORT" .!= Port 8030


defaultJwkUrl :: String
defaultJwkUrl = "https://mirza.au.auth0.com/.well-known/jwks.json"

--  ------------------------------------------------


--  ----------------Opts----------------------------


data AppMode
  = Proxy
  | UserManager
  | Bootstrap
  deriving (Show, Eq, Generic, Read)

instance Var AppMode where
  fromVar m = readMaybe m :: Maybe AppMode
  toVar = show

data Opts = Opts
  { myServiceInfo     :: ServiceInfo
  , scsServiceInfo    :: ServiceInfo
  , trailsServiceInfo :: ServiceInfo
  , appMode           :: AppMode
  , jwkUrl            :: String
  , jwkClientId       :: String
  , dbConnectionStr   :: ByteString
  } deriving (Show, Generic, Eq)

instance DefConfig Opts where
  defConfig = Opts
    { myServiceInfo     = ServiceInfo{serviceHost=Hostname "localhost", servicePort=Port 8080 }
    , scsServiceInfo    = ServiceInfo{serviceHost=Hostname "localhost", servicePort=Port 8000 }
    , trailsServiceInfo = ServiceInfo{serviceHost=Hostname "localhost", servicePort=Port 8030 }
    , appMode           = Proxy
    , jwkUrl            = defaultJwkUrl
    , jwkClientId       = ""
    , dbConnectionStr   = "dbname=deventitydataapi"
    }

instance FromEnv Opts where
  fromEnv = Opts
    <$> fromEnvMyServiceInfo
    <*> fromEnvSCSServiceInfo
    <*> fromEnvTrailsServiceInfo
    <*> envMaybe "EDAPI_MODE" .!= Proxy
    <*> envMaybe "JWK_URL" .!= defaultJwkUrl
    <*> env "JWK_CLIENT_IDS"
    <*> env "EDAPI_DB_CONN"
