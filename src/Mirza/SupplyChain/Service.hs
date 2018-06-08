{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

-- | Endpoint definitions go here. Most of the endpoint definitions are
-- light wrappers around functions in BeamQueries
module Mirza.SupplyChain.Service where



import           Mirza.SupplyChain.API
import qualified Mirza.SupplyChain.BeamQueries                as BQ
import           Mirza.SupplyChain.ErrorUtils                 (appErrToHttpErr)

import           Mirza.SupplyChain.Handlers.Business
import           Mirza.SupplyChain.Handlers.Common
import           Mirza.SupplyChain.Handlers.Contacts
import           Mirza.SupplyChain.Handlers.EventRegistration
import           Mirza.SupplyChain.Handlers.Queries
import           Mirza.SupplyChain.Handlers.Signatures        hiding
                                                               (getPublicKey)
import           Mirza.SupplyChain.Handlers.Users
import           Mirza.SupplyChain.Types                      hiding
                                                               (NewUser (..),
                                                               User (userId))
import qualified Mirza.SupplyChain.Types                      as ST

import           Servant
import           Servant.Swagger

import           GHC.TypeLits                                 (KnownSymbol)

import           Control.Lens                                 hiding ((.=))
import           Control.Monad.IO.Class                       (liftIO)
import qualified Data.HashMap.Strict.InsOrd                   as IOrd
import           Data.Swagger
import           Data.Text.Encoding                           (decodeUtf8)



appHandlers :: (SCSApp context err, HasScryptParams context) => ServerT ServerAPI (AppM context err)
appHandlers = publicServer :<|> privateServer

publicServer :: (SCSApp context err, HasScryptParams context) => ServerT PublicAPI (AppM context err)
publicServer =
  -- Users
       newUser
  -- Business
  :<|> getPublicKey
  :<|> getPublicKeyInfo
  :<|> listBusinesses

privateServer :: (SCSApp context err) => ServerT ProtectedAPI (AppM context err)
privateServer =
-- Contacts
       listContacts
  :<|> addContact
  :<|> removeContact
--  :<|> contactsSearch
  :<|> userSearch
-- Signatures
  :<|> addUserToEvent
  :<|> eventSign
  :<|> eventHashed
-- Queries
  :<|> epcState
  :<|> listEvents
  :<|> eventInfo
  :<|> eventList
  :<|> eventUserList
-- Event Registration
  :<|> insertObjectEvent
  :<|> insertAggEvent
  :<|> insertTransactEvent
  :<|> insertTransfEvent
-- Business
  :<|> addPublicKey


instance (KnownSymbol sym, HasSwagger sub) => HasSwagger (BasicAuth sym a :> sub) where
  toSwagger _ =
    let
      authSchemes = IOrd.singleton "basic" $ SecurityScheme SecuritySchemeBasic Nothing
      securityRequirements = [SecurityRequirement $ IOrd.singleton "basic" []]
    in
      toSwagger (Proxy :: Proxy sub)
      & securityDefinitions .~ authSchemes
      & allOperations . security .~ securityRequirements

-- 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: SCSContext -> BasicAuthCheck ST.User
authCheck context =
  let check (BasicAuthData useremail pass) = do
        eitherUser <- runAppM @_ @ServiceError context . runDb $
                      BQ.authCheck (EmailAddress $ decodeUtf8 useremail) (Password pass)
        case eitherUser of
          Right (Just user) -> return (Authorized user)
          _                 -> return Unauthorized
  in BasicAuthCheck check

appMToHandler :: forall x context. context -> AppM context AppError x -> Handler x
appMToHandler context act = do
  res <- liftIO $ runAppM context act
  case res of
    Left (AppError e) -> appErrToHttpErr e
    Right a           -> return a

-- | Swagger spec for server API.
serveSwaggerAPI :: Swagger
serveSwaggerAPI = toSwagger serverAPI
  & info.title   .~ "Supplychain Server API"
  & info.version .~ "1.0"
  & info.description ?~ "This is an API that tests swagger integration"
  & info.license ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")

-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: SCSContext -> Servant.Context '[BasicAuthCheck ST.User]
basicAuthServerContext context = authCheck context :. EmptyContext







-- PSUEDO:
-- In BeamQueries, implement a function getLabelIDState :: LabelEPCUrn -> IO (_labelID, State)
-- use readLabelEPC in EPC.hs to do it.
-- SELECT * FROM Labels WHERE _labelGs1CompanyPrefix=gs1CompanyPrefix AND _labelType=type AND ...


--eventHash :: EventId -> AppM context err SignedEvent
--eventHash eID = return (SignedEvent eID (BinaryBlob ByteString.empty) [(BinaryBlob ByteString.empty)] [1,2])
