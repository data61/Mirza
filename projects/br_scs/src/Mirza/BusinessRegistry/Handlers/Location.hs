{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


module Mirza.BusinessRegistry.Handlers.Location
  ( addLocation
  , getLocationByGLN
  , searchLocation
  , uxLocationByGLN
  , uxLocation
  ) where


import           Mirza.BusinessRegistry.Database.Schema   as DB
import qualified Mirza.BusinessRegistry.Handlers.Business as BRHB (searchBusinesses)
import           Mirza.BusinessRegistry.SqlUtils
import           Mirza.BusinessRegistry.Types             as BT
import           Mirza.Common.Time                        (toDbTimestamp)
import           Mirza.Common.Types                       (Member)
import           Mirza.Common.Utils


import           Data.GS1.EPC                             (GS1CompanyPrefix,
                                                           LocationEPC (..))

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions

import           GHC.Stack                                (HasCallStack,
                                                           callStack)

import           Katip

import           Network.URI                              (nullURI)

import           Control.Lens                             (( # ))
import           Control.Lens.Operators                   ((&))
import           Control.Monad                            (when)
import           Control.Monad.Error.Hoist                ((<!?>))
import           Data.Foldable                            (find, for_)
import           Data.Time                                (UTCTime)

import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (..),
                                                           constraintViolation)

import           Control.Monad.Except                     (throwError)

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
    Just userBizId -> do
      let pfx = _sglnCompanyPrefix . newLocGLN $ newLoc
          bizId = BizId pfx
      when (userBizId /= bizId) $
          throwing _OperationNotPermittedBRE (pfx, BT.UserId uId)
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
        { location_id          = locId
        , location_biz_id      = bizId
        , location_gln         = newLocGLN
        , location_last_update = Nothing
        }
      , GeoLocationT
        { geoLocation_id          = geoLocId
        , geoLocation_gln         = LocationId newLocGLN
        , geoLocation_latitude    = fst <$> newLocCoords
        , geoLocation_longitude   = snd <$> newLocCoords
        , geoLocation_address     = newLocAddress
        , geoLocation_last_update = Nothing
        }
    )


getLocationByGLN  :: ( Member context '[HasLogging, HasConnPool, HasEnvType]
                     , Member err     '[AsBRError, AsSqlError]
                     , HasCallStack)
                  => LocationEPC
                  -> AppM context err LocationResponse
getLocationByGLN gln = locationToLocationResponse
  <$> (runDb (getLocationByGLNQuery gln) <!?>  (_LocationNotKnownBRE # ()))


locationToLocationResponse :: (Location,GeoLocation) -> LocationResponse
locationToLocationResponse (LocationT{location_biz_id = BizId bizId,..} , GeoLocationT{..}) = LocationResponse
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
             & orderBy_ (desc_ . geoLocation_last_update)
  guard_ (primaryKey loc ==. val_ (LocationId gln))
  guard_ (geoLocation_gln geoloc ==. primaryKey loc)
  pure (loc,geoloc)


searchLocation :: (Member context '[HasDB]
                  , Member err    '[AsSqlError])
               => Maybe GS1CompanyPrefix
               -> Maybe UTCTime
               -> AppM context err [LocationResponse]
searchLocation mpfx mafter = fmap locationToLocationResponse
  <$> runDb (searchLocationQuery mpfx mafter)

searchLocationQuery :: Maybe GS1CompanyPrefix -> Maybe UTCTime -> DB context err [(Location,GeoLocation)]
searchLocationQuery mpfx mafter = pg $ runSelectReturningList $ select $ do

  loc    <- all_ (_locations businessRegistryDB)
  geoloc <- all_ (_geoLocations businessRegistryDB)
              & orderBy_ (desc_ . geoLocation_last_update)
              -- Temporarily remove the following constraint which restricts the search to the last entry added.
              -- This was causing a bug which effected the implementation of https://github.com/data61/Mirza/issues/340.
              -- This issue is being tracked with issue: https://github.com/data61/Mirza/issues/364
              -- & limit_ 1

  guard_ (geoLocation_gln geoloc `references_` loc)

  for_ mpfx $ \pfx -> do
    biz    <- all_ (_businesses businessRegistryDB)
    guard_ (location_biz_id loc `references_` biz)
    guard_ (val_ (BizId pfx) `references_` biz)

  for_ mafter $ \after ->
    guard_ (location_last_update loc       >=. just_ (val_ (toDbTimestamp after))
        ||. geoLocation_last_update geoloc >=. just_ (val_ (toDbTimestamp after)))

  pure (loc, geoloc)

uxLocationByGLN :: ( Member context '[HasDB, HasLogging]
                   , Member err     '[AsBRError, AsSqlError])
                => LocationEPC
                -> GS1CompanyPrefix
                -> AppM context err BusinessAndLocationResponse
uxLocationByGLN locEpc pfx = do
  loc <- getLocationByGLN locEpc
  biz <- BRHB.searchBusinesses (Just pfx) Nothing Nothing
  case biz of
    [b] -> pure $ BusinessAndLocationResponse b loc
    _   -> throwing_ _BusinessDoesNotExistBRE


-- The maximum number of companies that can be searched for in a single uxLocation query.
maxPrefixesForUxLocations :: Int
maxPrefixesForUxLocations = 25


uxLocation :: ( Member context '[HasDB, HasLogging]
              , Member err     '[AsSqlError])
           => [GS1CompanyPrefix]
           -> AppM context err [BusinessAndLocationResponse]
uxLocation userPrefixes = do
  -- We constrain the maximum number of company prefixes that can be quired in a single invocation to prevent abuse.
  let prefixes = take maxPrefixesForUxLocations userPrefixes
  locations <- traverse getLocations prefixes
  businesses <- traverse getBusinesses prefixes
  buildBusinessAndLocationResponses (concat businesses) (concat locations)

  where
    getLocations :: (Member context '[HasDB], Member err '[AsSqlError])
                 => GS1CompanyPrefix -> AppM context err [LocationResponse]
    getLocations prefix = searchLocation (Just prefix) Nothing

    getBusinesses :: (Member context '[HasDB], Member err '[AsSqlError])
                  => GS1CompanyPrefix -> AppM context err [BusinessResponse]
    getBusinesses prefix  = BRHB.searchBusinesses (Just prefix) Nothing Nothing

    matchId :: LocationResponse -> BusinessResponse -> Bool
    matchId location business = (locationBiz location) == (businessGS1CompanyPrefix business)

    buildBusinessAndLocationResponse :: Member context '[HasLogging]
                                     => [BusinessResponse] -> LocationResponse -> AppM context err BusinessAndLocationResponse
    buildBusinessAndLocationResponse businesses location =
      (\business -> BusinessAndLocationResponse business location) <$> matchedBusiness
      where
        -- It shouldn't be possible to get here, and indicates a logic error in our code
        -- or database. We don't want to break and error if we do since this is a pretty UX
        -- endpoint. Returning the mostly complete info and loggging the error so we can fix
        -- seems the most reasonable compromise.
        unfoundBusiness = do
          $(logTM) WarningS "Could not find a business that corresponds with this business location."
          pure (BusinessResponse (locationBiz location) "[Unknown]" nullURI)
        matchedBusiness = maybe unfoundBusiness pure $ find (matchId location) businesses

    buildBusinessAndLocationResponses :: Member context '[HasLogging]
                                      => [BusinessResponse] -> [LocationResponse] -> AppM context err [BusinessAndLocationResponse]
    buildBusinessAndLocationResponses businesses locations = sequence $ (buildBusinessAndLocationResponse businesses) <$> locations
