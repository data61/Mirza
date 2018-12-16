{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans            #-}

{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Mirza.Common.Types
  ( EmailAddress, emailToText, Password(..)  , UserId(..)
  , BRKeyId(..)
  , EnvType(..)
  , AppM(..)
  , runAppM
  , DB(..)
  , runDb
  , pg
  , Member
  , HasLogging
  , AsSqlError(..)
  , HasConnPool(..)
  , HasEnvType(..)
  , HasScryptParams(..)
  , HasKatipContext(..)
  , HasKatipLogEnv(..)
  , HasBRClientEnv(..)
  , HasDB
  , AsServantError (..)
  , DBConstraint
  , ask, asks
  , MonadError
  , throwing, throwing_
  , MonadIO, liftIO
  , PrimaryKeyType
  , brKeyIdType
  , runClientFunc
  , HealthResponse (..) , successHealthResponseText
  ) where

import qualified Database.Beam                        as B
import           Database.Beam.Backend.SQL            (FromBackendRow,
                                                       HasSqlValueSyntax)
import qualified Database.Beam.Backend.SQL            as BSQL
import           Database.Beam.Migrate.SQL            (DataType (..))
import           Database.Beam.Postgres               (Pg)
import           Database.Beam.Postgres.Syntax        (PgDataTypeSyntax,
                                                       pgUuidType)
import           Database.PostgreSQL.Simple           (Connection, SqlError)
import qualified Database.PostgreSQL.Simple           as DB
import           Database.PostgreSQL.Simple.FromField (FromField, fromField)
import           Database.PostgreSQL.Simple.ToField   (ToField, toField)

import           Data.Proxy                           (Proxy (..))

import qualified Control.Exception                    as Exc
import qualified Control.Exception                    as E
import           Control.Monad.Except                 (ExceptT (..), MonadError,
                                                       runExceptT, throwError)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       ask, asks, local,
                                                       runReaderT)
import           Control.Monad.Trans                  (lift)

import           Data.Pool                            as Pool

import           Crypto.JOSE                          (JWK, JWS, JWSHeader,
                                                       Signature)
import           Crypto.JOSE.Types                    (Base64Octets)
import           Crypto.Scrypt                        (ScryptParams)

import qualified Data.ByteString                      as BS
import           Data.Text                            (Text)
import           Data.Text.Encoding                   as T
import           Text.Email.Validate                  (EmailAddress,
                                                       toByteString, validate)

import           Data.Aeson
import           Data.Aeson.Types                     (typeMismatch)

import           Control.Lens
import           Control.Monad.Error.Lens


import           GHC.Exts                             (Constraint)
import           GHC.Generics                         (Generic)

import           Katip                                as K
import           Katip.Monadic                        (askLoggerIO)

import           Data.Swagger
import           Servant                              (FromHttpApiData (..),
                                                       ToHttpApiData (..))
import           Servant.Client                       (ClientEnv (..), ClientM,
                                                       ServantError (..),
                                                       runClientM)

import           Data.UUID                            (UUID)

type PrimaryKeyType = UUID



-- *****************************************************************************
-- Orphan Instances
-- *****************************************************************************

instance ToJSON EmailAddress where
  toJSON = toJSON . T.decodeUtf8 . toByteString

instance FromJSON EmailAddress where
  parseJSON = withText "EmailAddress" $ \t -> case validate (T.encodeUtf8 t) of
    Left err -> fail err
    Right e  -> pure e

instance ToSchema EmailAddress where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
    <&> name ?~ "Email address"
    <&> schema . description ?~ "An RFC 5322 email address"

emailToText :: EmailAddress -> Text
emailToText = decodeUtf8 . toByteString

-- *****************************************************************************
-- User Types
-- *****************************************************************************

-- TODO: Handwrite these instances to comply with their defined syntax
-- For example, emails have their own format, as do LabelEPCUrn
newtype UserId = UserId {getUserId :: PrimaryKeyType}
  deriving (Show, Eq, Generic, Read, Ord, FromJSON, ToJSON)
instance ToSchema UserId
instance ToParamSchema UserId
deriving instance FromHttpApiData UserId
deriving instance ToHttpApiData UserId

-- | Do NOT derive an `Eq` instance for Password. We do not want a literal
-- equality check for password
newtype Password = Password BS.ByteString

instance Show Password where
  show _ = "Password <redacted>"

newtype BRKeyId = BRKeyId {getBRKeyId :: UUID}
  deriving (Show, Eq, Generic, Read, FromJSON, ToJSON)
instance ToSchema BRKeyId
instance ToParamSchema BRKeyId
instance FromHttpApiData BRKeyId where
  parseUrlPiece t = fmap BRKeyId (parseUrlPiece t)
deriving instance ToHttpApiData BRKeyId

instance FromField BRKeyId where
  fromField field mbs = BRKeyId <$> fromField field mbs

instance ToField BRKeyId where
  toField = toField . getBRKeyId

instance HasSqlValueSyntax be UUID => HasSqlValueSyntax be BRKeyId where
    sqlValueSyntax (BRKeyId uuid) = BSQL.sqlValueSyntax uuid

instance (BSQL.BeamBackend be, FromBackendRow be UUID)
        => FromBackendRow be BRKeyId where
  fromBackendRow = BRKeyId <$> BSQL.fromBackendRow
  valuesNeeded proxyBE _proxyKID = BSQL.valuesNeeded proxyBE (Proxy :: Proxy UUID)

brKeyIdType :: DataType PgDataTypeSyntax BRKeyId
brKeyIdType = DataType pgUuidType


data EnvType = Prod | Dev
  deriving (Show, Eq, Read)

-- | The class of contexts which include an 'EnvType'
$(makeClassy ''EnvType)

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

-- =============================================================================
-- Classes and utilities for working with Constraints
-- =============================================================================


-- | Helper to make constraints on functions cleaner:
--
-- bazQuery :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
--             , Member err     '[AsBRError, AsBRKeyError])
--             => Foo
--             -> DB context err Bar
type family Member (e :: *) (cs :: [* -> Constraint]) :: Constraint where
  Member e '[] = ()
  Member e (c ': cs) = (c e, Member e cs)

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

instance AsSqlError SqlError where
  _SqlError = id

-- | The class of contexts which have Scrypt parameters
class HasScryptParams a where
  scryptParams :: Lens' a ScryptParams

-- Logging classes
-- ===============

-- | Convenience class for contexts which can be used for logging
-- @
--   foo :: Member context '[HasLogging] => Foo -> DB context err Bar
-- @
class (HasKatipContext context, HasKatipLogEnv context)
  => HasLogging context where
instance (HasKatipContext context, HasKatipLogEnv context)
  => HasLogging context


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



class HasBRClientEnv a where
  clientEnv :: Lens' a ClientEnv

class AsServantError a where
    _ServantError :: Prism' a ServantError


-- Useage of this type is deprecated prefer HasDb.
-- TODO: Remove DBConstraint once SCS is converted to use Member notation.
type DBConstraint context err =
    ( HasEnvType context
    , HasConnPool context
    , HasKatipContext context
    , HasKatipLogEnv context
    , AsSqlError err)

-- | Convenience class for contexts which require DB.
class (HasEnvType context, HasConnPool context, HasLogging context)
  => HasDB context where
instance (HasEnvType context, HasConnPool context, HasLogging context)
  => HasDB context


-- | Run a DB action within a transaction. See the documentation for
-- 'withTransaction'. SqlError exceptions will be caught and lifted into the
-- AppM MonadError instance, as will all app errors thrown in the DB a action,
-- and in either case the database transaction is rolled back.
--
-- Exceptions which are thrown which are not SqlErrors will be caught by Servant
-- and cause 500 errors (these are not exceptions we'll generally know how to
-- deal with).
runDb :: (HasDB context
         , Member err     '[AsSqlError])
      => DB context err a -> AppM context err a
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
runAppM env aM = runExceptT $ (runReaderT . getAppM) aM env


runClientFunc :: (AsServantError err, HasBRClientEnv context)
              => ClientM a
              -> AppM context err a
runClientFunc func = do
  cEnv <- view clientEnv
  either (throwing _ServantError) pure =<< liftIO (runClientM func cEnv)




-- TODO: Orphan for JWK

instance ToSchema JWK where
  declareNamedSchema _ = do
    strSchema <- declareSchemaRef (Proxy :: Proxy String)
    pure $ NamedSchema (Just "JWK") $ mempty
      & type_ .~ SwaggerObject
      & properties .~
          [ ("kty",strSchema)
          , ("n",strSchema)
          , ("e",strSchema)
          ]

instance ToSchema (JWS Identity () JWSHeader) where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "JWS") mempty

instance ToSchema (Signature () JWSHeader) where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "JWS Signature") mempty

instance ToSchema Base64Octets where
  declareNamedSchema _ =
    pure $ NamedSchema (Just "Base64 Encoded Bytes") $ mempty
      & type_ .~ SwaggerString

-- *****************************************************************************
-- Health Types
-- *****************************************************************************

successHealthResponseText :: Text
successHealthResponseText = "Status OK"

data HealthResponse = HealthResponse
  deriving (Show, Eq, Read, Generic)
instance ToSchema HealthResponse
instance ToJSON HealthResponse where
  toJSON _ = toJSON successHealthResponseText
instance FromJSON HealthResponse where
  parseJSON (String value)
    | value == successHealthResponseText = pure HealthResponse
    | otherwise                          = fail "Invalid health response string."
  parseJSON value                        = typeMismatch "HealthResponse" value

