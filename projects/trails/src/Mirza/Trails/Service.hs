{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


module Mirza.Trails.Service where


import           Mirza.Trails.API
import           Mirza.Trails.Handlers.Health
import           Mirza.Trails.Handlers.Trails
import           Mirza.Trails.Types

import           Mirza.Common.Types
import           Mirza.Common.Utils

import           Katip

import           Servant
import           Servant.Swagger

import           Control.Lens                 hiding ((.=))
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans

import           Data.ByteString.Lazy.Char8   as BSL8

import           Data.Swagger


-- Convenience class for contexts which require all possible error types that
-- could be thrown through the handlers.
class (AsTrailsError err, AsSqlError err)
  => APIPossibleErrors err where
instance (AsTrailsError err, AsSqlError err)
  => APIPossibleErrors err


appHandlers :: ( Member context '[HasDB]
               , APIPossibleErrors err)
            => ServerT ServerAPI (AppM context err)
appHandlers = publicServer

publicServer :: ( Member context '[HasDB]
                , APIPossibleErrors err)
             => ServerT PublicAPI (AppM context err)
publicServer =
       health
  :<|> versionInfo
  :<|> getTrail
  :<|> addTrail



appMToHandler :: (HasLogging context) => context -> AppM context TrailsError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ runAppM context act
  case res of
    Left err ->
      runKatipContextT (context ^. katipLogEnv) () (context ^. katipNamespace) (trailsErrorToHttpError err)
    Right a  -> pure a


-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Trails Registry Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")


errorLogLevel :: ServantErr -> Severity
errorLogLevel httpStatus
  | is5XXError(httpStatus) = ErrorS
  | otherwise = WarningS

-- | Is the servant error in the 5XX series?
is5XXError :: ServantErr -> Bool
is5XXError servantError = ((errHTTPCode servantError) `div` 100) == 5


-- | This function simplifies the construction of errors by providing an
-- interface with just the arguments necessary. This function logs the error
-- and if the the status code is in the 5XX the log level is escalated. We log
-- all errors for now so that developers have the oppertunity to skim the logs
-- to look for potential issues. The error type contains all the information
-- that we know about the error at this point so we add it in entirity to the
-- log.
-- TODO: Transform Show error so that we can only log OR and ORKeyErrors to
-- further constrain the type and prevent accidental errors in the argument
-- provided, even though all we need is show.
throwHttpError :: (Show error) => error -> ServantErr -> ByteString -> KatipContextT Handler a
throwHttpError err httpStatus errorMessage = do
  $(logTM) (errorLogLevel httpStatus) (logStr $ show err)
  lift $ throwError $ httpStatus { errBody = errorMessage }


-- | Takes a TrailsError and converts it to an HTTP error.
trailsErrorToHttpError :: TrailsError -> KatipContextT Handler a
trailsErrorToHttpError trailsError =
  let _httpError = throwHttpError trailsError
  in case trailsError of
    (DBErrorTE _)                  -> unexpectedError trailsError
    (UnmatchedUniqueViolationTE _) -> unexpectedError trailsError

-- | A generic internal server error has occured. We include no more information in the result returned to the user to
-- limit further potential for exploitation, under the expectation that we log the errors to somewhere that is reviewed
-- regularly so that the development team are informed and can identify and patch the underlying issues.
unexpectedError :: TrailsError -> KatipContextT Handler a
unexpectedError trailsError = throwHttpError trailsError err500 "An unknown error has occured."
