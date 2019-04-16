{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Mirza.EntityDataAPI.Database.Utils where

import           Database.PostgreSQL.Simple           as PG
import           Database.PostgreSQL.Simple.FromField (FromField (..))
import           Database.PostgreSQL.Simple.ToField   (ToField (..))

import           Mirza.EntityDataAPI.Types

import           Crypto.JWT                           (AsError, AsJWTError,
                                                       JWKSet, StringOrURI (..),
                                                       stringOrUri)
import qualified Crypto.JWT                           as JWT

import qualified Control.Exception                    as Exc

import           Control.Monad.Reader                 (asks, liftIO)

import           Data.Pool                            (withResource)

import           Data.Aeson

import qualified Data.Text                            as T

import           Control.Lens

doesSubExist :: StringOrURI -> AppM AuthContext AppError Bool
doesSubExist s = do
  pool <- asks dbConnPool
  liftIO $ withResource pool $ \conn -> do
    [Only cnt]  <- query conn "SELECT COUNT(*) FROM users WHERE user_sub = ?" [s] :: IO [Only Integer]
    case cnt of
      1 -> pure True
      _ -> pure False

unpackStringOrURI :: StringOrURI -> String
unpackStringOrURI sUri =
  let (String str) = toJSON sUri
    in T.unpack str

instance ToField StringOrURI where
  toField = toField . unpackStringOrURI

instance FromField StringOrURI where
  fromField s = error "not implemented yet"


-- -- runDb :: AppM AuthContext AppError a
-- runDb act = do
--   pool <- dbConnPool asks
--   res <- withResource pool $ \conn ->


-- runDb (DB act) = do
--   env <- ask
--   e <- view envType

--   res <- liftIO $ withResource (dbConnPool env) $ \conn ->
--           Exc.try
--          . withTransaction conn
--          . dbf conn
--          . runExceptT
--          . runReaderT act $ (conn,env)
--         -- :: AppM (Either SqlError (Either AppError a))
--   either (throwing _SqlError)
--          (either throwError pure)
--          res
