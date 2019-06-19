{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}


module Mirza.OrgRegistry.Handlers.Location
  ( addLocation
  , getLocationByGLN
  , searchLocation
  , searchOrgLocationByGLN
  , searchOrgLocation
  ) where


import           Mirza.Common.Time                        (toDbTimestamp)
import           Mirza.Common.Types                       (Member)
import           Mirza.Common.Utils
import           Mirza.OrgRegistry.Auth
import           Mirza.OrgRegistry.Database.Schema        as DB
import qualified Mirza.OrgRegistry.Handlers.Org           as ORHO (searchOrgs)
import           Mirza.OrgRegistry.SqlUtils
import           Mirza.OrgRegistry.Types                  as ORT


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
import           Control.Monad.Error.Hoist                ((<!?>))
import           Data.Foldable                            (find, for_)
import           Data.Time                                (UTCTime)


addLocation :: ( Member context '[HasEnvType, HasConnPool, HasLogging]
               , Member err     '[AsSqlError, AsORError])
            => AuthUser
            -> NewLocation
            -> AppM context err LocationId
addLocation auser newLoc = do
  newLocId <- newUUID
  newGeoLocId <- newUUID
  (fmap primaryKey)
    . (handleError (transformSqlUniqueViloation "location_pkey" (\_sqlerr -> _LocationExistsORE # ())))
    . runDb
    . addLocationQuery auser newLocId (GeoLocationId newGeoLocId)
    $ newLoc
  -- TODO: discover which constraints are needed and what we should catch here
  -- (awaiting tests)
  -- where
  --   errHandler :: (AsSqlError err, AsORError err, MonadError err m, MonadIO m) => err -> m a
  --   errHandler e = case e ^? _SqlError of
  --     Nothing -> throwError e
  --     Just sqlErr ->
  --       case constraintViolation sqlErr of
  --         Just (UniqueViolation "orgs_pkey") -> throwing_ _GS1CompanyPrefixExistsORE
  --         _ -> throwError e

addLocationQuery  :: ( Member context '[]
                     , Member err     '[AsORError])
                  => AuthUser
                  -> PrimaryKeyType
                  -> GeoLocationId
                  -> NewLocation
                  -> DB context err Location
addLocationQuery authUser locId geoLocId newLoc = do
  let gs1CompanyPrefix = _sglnCompanyPrefix $ newLocGLN newLoc
  mapping <- userOrganisationAuthorisationQuery authUser gs1CompanyPrefix
  let orgId = org_mapping_gs1_company_prefix mapping
  let (loc,geoLoc) = newLocationToLocation locId geoLocId orgId newLoc
  res <- pg $ runInsertReturningList (_locations orgRegistryDB) $ insertValues [loc]
  case res of
    [r] -> do
           _ <- pg $ runInsertReturningList (_geoLocations orgRegistryDB) $
               insertValues [geoLoc]
           pure r
    _   -> throwing _UnexpectedErrorORE callStack


newLocationToLocation :: PrimaryKeyType
                      -> GeoLocationId
                      -> OrgId
                      -> NewLocation
                      -> (Location, GeoLocation)
newLocationToLocation
  locId (GeoLocationId geoLocId) orgId
  NewLocation{newLocGLN, newLocCoords, newLocAddress} =
    ( LocationT
        { location_id          = locId
        , location_org_id      = orgId
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
                     , Member err     '[AsORError, AsSqlError]
                     , HasCallStack)
                  => LocationEPC
                  -> AppM context err LocationResponse
getLocationByGLN gln = locationToLocationResponse
  <$> (runDb (getLocationByGLNQuery gln) <!?>  (_LocationNotKnownORE # ()))


locationToLocationResponse :: (Location,GeoLocation) -> LocationResponse
locationToLocationResponse (LocationT{location_org_id = OrgId orgId,..} , GeoLocationT{..}) = LocationResponse
  { locationId    = location_id
  , locationGLN   = location_gln
  , locationOrg   = orgId
  , geoLocId      = geoLocation_id
  , geoLocCoord   = (,) <$> geoLocation_latitude <*> geoLocation_longitude
  , geoLocAddress = geoLocation_address
  }


getLocationByGLNQuery :: ( Member context '[]
                         , Member err     '[AsORError])
                         => LocationEPC
                         -> DB context err (Maybe (Location, GeoLocation))
getLocationByGLNQuery gln = pg $ runSelectReturningOne $ select $ do
  loc   <- all_ (_locations orgRegistryDB)
  geoloc <- all_ (_geoLocations orgRegistryDB)
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

  loc    <- all_ (_locations orgRegistryDB)
  geoloc <- all_ (_geoLocations orgRegistryDB)
              & orderBy_ (desc_ . geoLocation_last_update)
              -- Temporarily remove the following constraint which restricts the search to the last entry added.
              -- This was causing a bug which effected the implementation of https://github.com/data61/Mirza/issues/340.
              -- This issue is being tracked with issue: https://github.com/data61/Mirza/issues/364
              -- & limit_ 1

  guard_ (geoLocation_gln geoloc `references_` loc)

  for_ mpfx $ \pfx -> do
    org    <- all_ (_orgs orgRegistryDB)
    guard_ (location_org_id loc `references_` org)
    guard_ (val_ (OrgId pfx) `references_` org)

  for_ mafter $ \after ->
    guard_ (location_last_update loc       >=. just_ (val_ (toDbTimestamp after))
        ||. geoLocation_last_update geoloc >=. just_ (val_ (toDbTimestamp after)))

  pure (loc, geoloc)

searchOrgLocationByGLN :: ( Member context '[HasDB, HasLogging]
                   , Member err     '[AsORError, AsSqlError])
                => LocationEPC
                -> GS1CompanyPrefix
                -> AppM context err OrgAndLocationResponse
searchOrgLocationByGLN locEpc pfx = do
  loc <- getLocationByGLN locEpc
  org <- ORHO.searchOrgs (Just pfx) Nothing Nothing
  case org of
    [b] -> pure $ OrgAndLocationResponse b loc
    _   -> throwing_ _OrgDoesNotExistORE


-- The maximum number of companies that can be searched for in a single uxLocation query.
maxPrefixesForUxLocations :: Int
maxPrefixesForUxLocations = 25


searchOrgLocation :: ( Member context '[HasDB, HasLogging]
              , Member err     '[AsSqlError])
           => [GS1CompanyPrefix]
           -> AppM context err [OrgAndLocationResponse]
searchOrgLocation userPrefixes = do
  -- We constrain the maximum number of company prefixes that can be quired in a single invocation to prevent abuse.
  let prefixes = take maxPrefixesForUxLocations userPrefixes
  locations <- traverse getLocations prefixes
  orgs <- traverse getOrgs prefixes
  buildOrgAndLocationResponses (concat orgs) (concat locations)

  where
    getLocations :: (Member context '[HasDB], Member err '[AsSqlError])
                 => GS1CompanyPrefix -> AppM context err [LocationResponse]
    getLocations prefix = searchLocation (Just prefix) Nothing

    getOrgs :: (Member context '[HasDB], Member err '[AsSqlError])
                  => GS1CompanyPrefix -> AppM context err [OrgResponse]
    getOrgs prefix  = ORHO.searchOrgs (Just prefix) Nothing Nothing

    matchId :: LocationResponse -> OrgResponse -> Bool
    matchId location org = (locationOrg location) == (orgGS1CompanyPrefix org)

    buildOrgAndLocationResponse :: Member context '[HasLogging]
                                     => [OrgResponse] -> LocationResponse -> AppM context err OrgAndLocationResponse
    buildOrgAndLocationResponse orgs location =
      (\org -> OrgAndLocationResponse org location) <$> matchedOrg
      where
        -- It shouldn't be possible to get here, and indicates a logic error in our code
        -- or database. We don't want to break and error if we do since this is a pretty UX
        -- endpoint. Returning the mostly complete info and loggging the error so we can fix
        -- seems the most reasonable compromise.
        unfoundOrg = do
          $(logTM) WarningS "Could not find a org that corresponds with this org location."
          pure (OrgResponse (locationOrg location) "[Unknown]" nullURI)
        matchedOrg = maybe unfoundOrg pure $ find (matchId location) orgs

    buildOrgAndLocationResponses :: Member context '[HasLogging]
                                      => [OrgResponse] -> [LocationResponse] -> AppM context err [OrgAndLocationResponse]
    buildOrgAndLocationResponses orgs locations = sequence $ (buildOrgAndLocationResponse orgs) <$> locations
