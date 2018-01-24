
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module AppConfig (
  AppM (..)
  , dbFunc
  , runDb
  , mkEnvType
  , Env(..)
)
where

import           Database.PostgreSQL.Simple (Connection)
import qualified Database.Beam as B
import           Database.Beam.Postgres (Pg)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT,
                                         asks, ask, liftIO)
-- import           GHC.Word               (Word16)
import           Servant.Server (Handler)
import           Control.Monad.Except (MonadError)
import           Model (DBError)


data EnvType = Prod | Dev

mkEnvType :: Bool -> EnvType
mkEnvType False = Prod
mkEnvType _ = Dev

data Env = Env
  { envType :: EnvType
  , dbConn  :: Connection
  -- , port    :: Word16
  }

newtype AppM a = AppM
  { unAppM :: ReaderT Env Handler a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadIO
          --  , MonadError DBError
           )

getDBConn :: AppM Connection
getDBConn = asks dbConn

getEnvType :: AppM EnvType
getEnvType = asks envType

-- |for the moment, comment in/out the appropriate line to the get the proper
-- function
-- dbFunc :: Connection -> (Pg a0 -> IO a0)
dbFunc :: AppM (Pg a0 -> IO a0)
-- dbFunc = withDatabase
dbFunc = do
  conn <- getDBConn
  e <- getEnvType
  case e of
    Prod -> pure $ B.withDatabase conn
    _    -> pure $ B.withDatabaseDebug putStrLn conn

-- | Helper function to run db functions
runDb :: Pg b -> AppM b
runDb q = dbFunc >>= (\f -> liftIO $ f q)

