{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DataKinds             #-}


module Mirza.BusinessRegistry.Handlers.Location
  ( addLocation
  , removeLocation
  ) where


import           Mirza.BusinessRegistry.Database.Schema   as DB
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Utils
import           Mirza.Common.Types                       (Member)

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
-- import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (UniqueViolation),
--                                                            constraintViolation)

import           Control.Lens                             ((#))

import           GHC.Stack                                (HasCallStack, callStack)
-- import           Control.Monad.Except                     (catchError, throwError)
import           Control.Monad.Error.Hoist                (hoistErrorM)


import           Servant (NoContent(NoContent))


addLocation :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
               , Member err     '[AsSqlError, AsBusinessRegistryError])
            => AuthUser
            -> NewLocation
            -> AppM context err LocationId
addLocation auser newLoc = do
  newLocId <- newUUID
  (fmap primaryKey)
    -- . (`catchError` errHandler)
    . runDb
    . addLocationQuery auser (LocationId newLocId)
    $ newLoc
  -- TODO: discover which constraints are needed and what we should catch here
  -- (awaiting tests)
  -- where
  --   errHandler :: (AsSqlError err, AsBusinessRegistryError err, MonadError err m, MonadIO m) => err -> m a
  --   errHandler e = case e ^? _SqlError of
  --     Nothing -> throwError e
  --     Just sqlErr ->
  --       case constraintViolation sqlErr of
  --         Just (UniqueViolation "businesses_pkey") -> throwing_ _GS1CompanyPrefixExistsBRE
  --         _ -> throwError e

addLocationQuery  :: ( Member context '[]
                     , Member err     '[AsBusinessRegistryError]
                     , HasCallStack)
                  => AuthUser
                  -> LocationId
                  -> NewLocation
                  -> DB context err Location
addLocationQuery (AuthUser (BT.UserId uId)) locId newLoc = do
  mbizId <- pg $ runSelectReturningOne $ select $ do
    user <- all_ (_users businessRegistryDB)
    guard_ (user_id user ==. val_ uId)
    pure $ user_biz_id user
  case mbizId of
               -- Since the user has authenticated, this should never happen
    Nothing -> throwing _UnexpectedErrorBRE callStack
    Just bizId -> do
      res <- pg $ 
        runInsertReturningList (_locations businessRegistryDB) $
        insertValues [newLocationToLocation locId bizId newLoc]
      case res of
        [r] -> pure r
        _   -> throwing _UnexpectedErrorBRE callStack


newLocationToLocation :: LocationId -> BizId -> NewLocation -> Location
newLocationToLocation (LocationId locId) bizId NewLocation {newLocGLN, newLocCoords, newLocAddress}
  = LocationT
    { location_id        = locId
    , location_biz_id    = bizId
    , location_gln       = newLocGLN
    , location_latitude  = fst <$> newLocCoords
    , location_longitude = snd <$> newLocCoords
    , location_address   = newLocAddress
    }

removeLocation :: (Member context '[HasEnvType, HasConnPool, HasLogging]
                  , Member err    '[AsSqlError, AsBusinessRegistryError]) 
               => AuthUser
               -> LocationId
               -> AppM context err NoContent
removeLocation auser locId = NoContent <$ runDb (removeLocationQuery auser locId)

removeLocationQuery :: (Member context '[]
                      , Member err     '[AsBusinessRegistryError]) 
                    => AuthUser
                    -> LocationId
                    -> DB context err [Location]
removeLocationQuery (AuthUser (BT.UserId auser)) (LocationId locId) = do
  _ <- hoistErrorM (_LocationRemovalErrorBRE #) $
        pg $ runSelectReturningOne $ select $ do
          user <- all_ (_users businessRegistryDB)
          loc  <- all_ (_locations businessRegistryDB)
          guard_ (user_id user ==. val_ auser
                  &&. location_id loc ==. val_ locId
                  &&. user_biz_id user ==. location_biz_id loc)
          pure user

      -- TODO: Check that user is member of business owning location
       -- see Database.Beam.Backend.SQL.BeamExtensions.runDeleteReturningList
       -- and subquery_ 

  pg $ runDeleteReturningList 
        (_locations businessRegistryDB) 
        (\loc -> location_id loc ==. val_ locId)
