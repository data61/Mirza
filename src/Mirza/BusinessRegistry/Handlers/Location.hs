{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DataKinds             #-}


module Mirza.BusinessRegistry.Handlers.Location
  ( addLocation
  ) where


import           Mirza.BusinessRegistry.Database.Schema   as DB
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Utils
import           Mirza.Common.Types                       (Member)

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           GHC.Stack                                (HasCallStack, callStack)


addLocation :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
               , Member err     '[AsSqlError, AsBusinessRegistryError])
            => AuthUser
            -> NewLocation
            -> AppM context err LocationId
addLocation auser newLoc = do
  newLocId <- newUUID
  newGeoLocId <- newUUID
  (fmap primaryKey)
    -- . (`catchError` errHandler)
    . runDb
    . addLocationQuery auser newLocId (GeoLocationId newGeoLocId)
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
                  -> PrimaryKeyType
                  -> GeoLocationId
                  -> NewLocation
                  -> DB context err Location
addLocationQuery (AuthUser (BT.UserId uId)) locId geoLocId newLoc = do
  mbizId <- pg $ runSelectReturningOne $ select $ do
    user <- all_ (_users businessRegistryDB)
    guard_ (user_id user ==. val_ uId)
    pure $ user_biz_id user
  case mbizId of
               -- Since the user has authenticated, this should never happen
    Nothing -> throwing _UnexpectedErrorBRE callStack
    Just bizId -> do
      let (loc,geoLoc) = newLocationToLocation locId geoLocId bizId newLoc
      res <- pg $ runInsertReturningList (_locations businessRegistryDB) $
                  insertValues [loc]
      case res of
        [r] -> do

            _ <- pg $ runInsertReturningList (_geoLocation businessRegistryDB) $
                 insertValues [geoLoc]
            pure r
        _   -> throwing _UnexpectedErrorBRE callStack


newLocationToLocation :: PrimaryKeyType 
                      -> GeoLocationId
                      -> BizId 
                      -> NewLocation 
                      -> (Location, GeoLocation)
newLocationToLocation 
  locId (GeoLocationId geoLocId) bizId 
  NewLocation{newLocGLN, newLocCoords, newLocAddress} =
    ( LocationT
        { location_id        = locId
        , location_biz_id    = bizId
        , location_gln       = newLocGLN
        }
      , GeoLocationT 
        { geoLocation_id        = geoLocId
        , geoLocation_gln       = LocationId newLocGLN
        , geoLocation_latitude  = fst <$> newLocCoords
        , geoLocation_longitude = snd <$> newLocCoords
        , geoLocation_address   = newLocAddress
        }
    )
