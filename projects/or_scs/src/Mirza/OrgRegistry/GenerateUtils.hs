{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-- | Utility functions to generate test data using
-- each service's client
module Mirza.OrgRegistry.GenerateUtils where

import           Mirza.Common.Utils      (mockURI)
import           Mirza.OrgRegistry.Types as ORT

import           Data.GS1.EPC

import qualified Data.Text               as T


dummyOrg :: T.Text -> NewOrg
dummyOrg unique = NewOrg (GS1CompanyPrefix ("Org" <> unique <> "Prefix"))
                                   ("Org" <> unique <> "Name")
                                   (mockURI unique)


dummyUser :: T.Text -> NewUser
dummyUser unique = NewUser $ OAuthSub $ "User" <> unique <> "OAuthSub"


generateMultipleUsers :: [T.Text] -> [NewUser]
generateMultipleUsers = fmap dummyUser
