{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Mirza.Common.Utils
  ( toText
  , randomText
  , notImplemented
  , newUUID
  , handleError
  , transformSqlUniqueViloationTemplate
  , handleSqlUniqueViloationTemplate
  , fromPgJSON
  , addLastUpdateTriggers
  , getTableNames
  , runClient
  , manager'
  , readJWK
  , expectRight
  , expectJust
  , expectUser
  , unsafeMkEmailAddress
  , mockURI
  , versionInfo
  , fetchJWKS
  , fetchJWKSWithManager
  ) where


import qualified Mirza.Common.Types                as CT


import           Development.GitRev
import           Text.Email.Validate               (EmailAddress, emailAddress)

import           Katip

import           Database.Beam.Postgres            (PgJSON (..), Postgres)
import           Database.Beam.Schema.Tables

import           Database.PostgreSQL.Simple        (SqlError (..), execute_)
import           Database.PostgreSQL.Simple.Errors (ConstraintViolation (UniqueViolation),
                                                    constraintViolation)

import           Servant.Client                    hiding (manager)

import           Data.Default

import           Crypto.JOSE                       (JWK, JWKSet)

import           Data.Aeson                        (Result (..), Value (..),
                                                    decodeFileStrict, fromJSON)
import           Data.Aeson.Lens

import           Network.HTTP.Client               (Manager, newManager)
import qualified Network.HTTP.Client               as C
import           Network.HTTP.Client.TLS
import           Network.HTTP.Req
import           Network.URI                       hiding (path)

import           Control.Lens                      (over, re, view, (^.), (^?),
                                                    _1)

import           Control.Monad                     (replicateM, (<$!>))
import           Control.Monad.Except              (MonadError, catchError,
                                                    throwError)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Writer
import           Data.Functor                      ((<$))
import           Data.Proxy                        (Proxy (..))

import           Control.Exception                 (try)
import           System.Random                     (randomRIO)

import           Data.ByteString.Base16            as B16
import           Data.ByteString.Base64.URL        as B64

import           Data.UUID                         (UUID)
import           Data.UUID.V4                      (nextRandom)

import           Data.ByteString                   (ByteString)
import           Data.Either                       (fromRight)
import           Data.Maybe                        (fromJust, fromMaybe)
import           Data.String                       (fromString)
import           Data.Text                         (Text, pack, unpack)
import           Data.Text.Encoding
import           Data.Text.Strict.Lens             (packed, utf8)

import           GHC.Stack                         (HasCallStack)
import           System.IO.Unsafe                  (unsafePerformIO)


versionInfo :: CT.AppM context err String
versionInfo = pure $(gitHash)

-- | Converts anything to a ``Text``
toText :: Show a => a -> Text
toText = pack . show

randomText :: IO Text
randomText = do
  count <- randomRIO (8 :: Int, 32)
  randomString <- (take count) <$> replicateM count (randomRIO ('a', 'z'))
  pure $ pack randomString

{-# WARNING notImplemented "notImplemented should not be used" #-}
notImplemented :: HasCallStack => a
notImplemented = error "FIXME"

-- | Generate a new v4 UUID - when being used in a database transaction,
-- this should be called inside the DB monad so that if the transaction
-- is retried a new UUID will be generated.
newUUID :: MonadIO m => m UUID
newUUID = liftIO nextRandom

-- | Ueful for handling specific errors from, for example, database transactions
-- @
--  handleError errHandler $ runDb ...
--  ...
--  where errHandler (AppError (DatabaseError sqlErr)) = ...
--        errHandler e = throwError e
-- @
handleError :: MonadError err m => (err -> m a) -> m a -> m a
handleError = flip catchError

transformSqlUniqueViloationTemplate
  :: (CT.AsSqlError err, MonadError err m, MonadIO m)
  => (SqlError -> err) -- ^ Handles every other unique constraint violation
  -> ByteString        -- ^ UniqueViolation name.
  -> (SqlError -> err) -- ^ A function which takes the original SQL error for the
                        --   UniqueViolation and turns it into the error that is thrown
                        --   when the UniqueViolation name is matched.
  -> err               -- ^ The error that we are catching.
  -> m a
transformSqlUniqueViloationTemplate f expectedName uniqueViolationError =
  handleSqlUniqueViloationTemplate f expectedName (throwError . uniqueViolationError)

handleSqlUniqueViloationTemplate
  :: (CT.AsSqlError err, MonadError err m, MonadIO m)
  => (SqlError -> err) -- ^ Handles every other unique constraint violation
  -> ByteString        -- ^ UniqueViolation name.
  -> (SqlError -> m a) -- ^ A function which takes the original SQL error for the
                        --   UniqueViolation and handles what should happen in this case.
  -> err               -- ^ The error that we are catching.
  -> m a
handleSqlUniqueViloationTemplate f expectedName action e = case e ^? CT._SqlError of
  Nothing -> throwError e
  Just sqlError ->
    case constraintViolation sqlError of
      Just (UniqueViolation violationName)
        | violationName == expectedName -> action sqlError
        | otherwise -> throwError (f sqlError)
      _ -> throwError e

fromPgJSON :: PgJSON a -> a
fromPgJSON (PgJSON x) = x



getTableNames :: Database Postgres db => DatabaseSettings Postgres db -> [Text]
getTableNames db = execWriter $ zipTables (Proxy :: Proxy Postgres)
    (\(DatabaseEntity desc) _ -> undefined <$ tell [desc ^. dbEntityName])
    db
    db

-- Adds triggers to all tables to set the last_update field to NOW(); on
-- INSERT and UPDATE.
-- See: https://stackoverflow.com/q/8740792
addLastUpdateTriggers :: (CT.HasLogging context
                         ,Database Postgres db)
                      => DatabaseSettings Postgres db
                      -> CT.DB context err ()
addLastUpdateTriggers db = forM_ (getTableNames db) $ \tName -> do
  conn <- view _1
  $(logTM) InfoS . logStr $ "Adding triggers to: " <> tName
  liftIO $ execute_ conn $ fromString $ unpack $
    "CREATE OR REPLACE FUNCTION sync_lastmod() RETURNS trigger AS $$ \
      \BEGIN \
        \NEW.last_update := NOW() AT TIME ZONE 'UTC'; \
        \RETURN NEW; \
      \END; \
      \$$ LANGUAGE plpgsql; \
      \DROP TRIGGER IF EXISTS sync_lastmod ON \"" <> tName <> "\";" <>
      "CREATE TRIGGER sync_lastmod \
      \BEFORE UPDATE OR INSERT ON \"" <> tName <>
        "\" FOR EACH ROW EXECUTE PROCEDURE sync_lastmod();"

{-# NOINLINE manager' #-}
manager' :: C.Manager
manager' = unsafePerformIO $ C.newManager C.defaultManagerSettings

runClient :: BaseUrl -> ClientM a -> IO (Either ServantError a)
runClient baseUrl' x = runClientM x (mkClientEnv manager' baseUrl')

-- Read a JWK key from file (either prubli or private).
readJWK :: FilePath -> IO (Maybe JWK)
readJWK = decodeFileStrict

-- GHC 8.6.x enabled MonadFailDesugaring by default, which means that if we're being lazy and
-- using incomplete pattern matches in a Monad which does have an instance for MonadFail
-- the code will will fail to compile. An example monad is the servant ClientM, therefore
-- expectJust/expectRight should be used when writing test code that expects either a Just/Right.

-- | Like fromJust from Data.Maybe except an error is thrown WITH a stacktrace
expectJust :: HasCallStack => Maybe a -> a
expectJust = fromMaybe (error "Expected: Just a")

-- | Returns b if Right, otherwise calls error
expectRight :: Either a b -> b
expectRight = fromRight (error "Expected: Right b")

-- | Same as expectRight but specalised for the type UserId with a more descript error message.
expectUser :: Either a CT.UserId -> CT.UserId
expectUser = fromRight (error "Expected: UserId")

--------------------------------------------------------------------------------
-- Email Utils
--------------------------------------------------------------------------------

-- | Only use this with hardcoded email addresses that are guaranteed to return
-- a ``Just``
unsafeMkEmailAddress :: ByteString -> EmailAddress
unsafeMkEmailAddress = fromJust . emailAddress


-- | Create a mock URI based on some unique text.
mockURI :: Text -> Network.URI.URI
mockURI unique = (Network.URI.URI "http:" (Just (URIAuth "" "example.com" "")) path "" "") where
                 path = Network.URI.escapeURIString Network.URI.isUnescapedInURI ("/" <> unpack unique)


data FetchJWKsError =
    UrlParseFailed
  | ReqFailure HttpException
  | JWKParseFailure [Char]
  deriving Show

fetchJWKS :: String -> IO (Either FetchJWKsError JWKSet)
fetchJWKS url = do
  manager <- newManager tlsManagerSettings
  fetchJWKSWithManager manager url

fetchJWKSWithManager :: Manager -> String -> IO (Either FetchJWKsError JWKSet)
fetchJWKSWithManager m url =
  case parseUrlHttps (url ^. packed . re utf8) of
    Nothing   -> pure $ Left UrlParseFailed
    Just url' -> ((toJWKS =<<) . (fmap Network.HTTP.Req.responseBody)) <$!> (mkReq url')
    where
      mkReq url' = do
        res <- try $ runReq (def { httpConfigAltManager = Just m }) $ req GET (url' ^. _1) NoReqBody jsonResponse mempty
        -- TODO:             ^ def will change to defaultHttpConfig once req-2.0 is in stackage LTS.
        case res of
          Left (e :: HttpException) -> pure . Left  $ ReqFailure e
          Right v                   -> pure . Right $ v

      -- Change the x5t to be spec compliant
      toJWKS :: Value -> Either FetchJWKsError JWKSet
      toJWKS v = let v' = over (key "keys" . values . key "x5t" . _String) b64HexToB64 v in
        case fromJSON v' of
          Error e   -> Left $ JWKParseFailure e
          Success a -> Right a
      b64HexToB64 :: Text -> Text
      b64HexToB64 = decodeUtf8 . B64.encode . fst . B16.decode . B64.decodeLenient . encodeUtf8
