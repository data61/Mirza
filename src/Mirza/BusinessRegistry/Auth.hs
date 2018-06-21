{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module Mirza.BusinessRegistry.Auth
  (
    basicAuthServerContext
  , authCheck
  ) where

import qualified Mirza.BusinessRegistry.Types as BT
import           Mirza.Common.Types           as CT
import           Mirza.SupplyChain.Types      (ServiceError)

import           Database.Beam                as B

import           Servant

import qualified Crypto.Scrypt                as Scrypt

import           Control.Lens                 (view, _2)
import           Data.Text.Encoding           (decodeUtf8)


-- | We need to supply our handlers with the right Context. In this case,
-- Basic Authentication requires a Context Entry with the 'BasicAuthCheck' value
-- tagged with "foo-tag" This context is then supplied to 'server' and threaded
-- to the BasicAuth HasServer handlers.
basicAuthServerContext :: (HasScryptParams context, DBConstraint context ServiceError)
                       => context  -> Servant.Context '[BasicAuthCheck BT.User]
basicAuthServerContext context = authCheck context :. EmptyContext


-- 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
-- authCheck :: SCSContext -> BasicAuthCheck ST.User
authCheck :: (HasScryptParams context, DBConstraint context ServiceError)
          => context -> BasicAuthCheck BT.User
authCheck context = BasicAuthCheck (\ basicAuthData -> pure (Authorized (BT.User ())))


