{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RecordWildCards       #-}


module Mirza.BusinessRegistry.Handlers.Location
  ( addLocation
  , getLocationByGLN
  ) where


import           Mirza.BusinessRegistry.Database.Schema   as DB
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Utils
import           Mirza.Common.Types                       (Member)
import           Mirza.BusinessRegistry.SqlUtils

import           Data.GS1.EPC                             (LocationEPC)

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           GHC.Stack                                (HasCallStack, callStack)

import           Control.Lens                             ((#))

addLocation :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
               , Member err     '[AsSqlError, AsBRError])
            => AuthUser
            -> NewLocation
            -> AppM context err LocationId
addLocation auser newLoc = do
  newLocId <- newUUID
  newGeoLocId <- newUUID
  (fmap primaryKey)
    . (handleError (handleSqlUniqueViloation "location_pkey" (\_sqlerr -> _LocationExistsBRE # ())))
    . runDb
    . addLocationQuery auser newLocId (GeoLocationId newGeoLocId)
    $ newLoc
  -- TODO: discover which constraints are needed and what we should catch here
  -- (awaiting tests)
  -- where
  --   errHandler :: (AsSqlError err, AsBRError err, MonadError err m, MonadIO m) => err -> m a
  --   errHandler e = case e ^? _SqlError of
  --     Nothing -> throwError e
  --     Just sqlErr ->
  --       case constraintViolation sqlErr of
  --         Just (UniqueViolation "businesses_pkey") -> throwing_ _GS1CompanyPrefixExistsBRE
  --         _ -> throwError e

addLocationQuery  :: ( Member context '[]
                     , Member err     '[AsBRError]
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

            _ <- pg $ runInsertReturningList (_geoLocations businessRegistryDB) $
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


getLocationByGLN :: ( Member context '[HasLogging, HasConnPool, HasEnvType]
                    , Member err     '[AsBRError, AsSqlError]
                    , HasCallStack)
                    => AuthUser
                    -> LocationEPC
                    -> AppM context err LocationResponse
getLocationByGLN _user gln = do
  res <- runDb $ getLocationByGLNQuery gln
  case res of
    Nothing -> throwing_ _LocationNotKnownBRE
    Just (LocationT{location_biz_id = BizId bizId,..} , GeoLocationT{..}) -> pure $ LocationResponse
      { locationId    = location_id
      , locationGLN   = location_gln
      , locationBiz   = bizId
      , geoLocId      = geoLocation_id
      , geoLocCoord   = (,) <$> geoLocation_latitude <*> geoLocation_longitude
      , geoLocAddress = geoLocation_address
      }


getLocationByGLNQuery :: ( Member context '[]
                         , Member err     '[AsBRError])
                         => LocationEPC
                         -> DB context err (Maybe (Location, GeoLocation))
getLocationByGLNQuery gln = pg $ runSelectReturningOne $ select $ do
  loc   <- all_ (_locations businessRegistryDB)
  geoloc <- all_ (_geoLocations businessRegistryDB)
  guard_ (primaryKey loc ==. val_ (LocationId gln))
  guard_ (geoLocation_gln geoloc ==. primaryKey loc)
  -- TODO: Add ORDER BY when we have a date modified field
  pure (loc,geoloc)