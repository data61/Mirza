{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-- | Utility functions to generate test data using
-- each service's client
module Mirza.BusinessRegistry.GenerateUtils where

import           Mirza.BusinessRegistry.Types as BT
import           Mirza.Common.Utils           (mockURI)

import           Data.GS1.EPC

import qualified Data.Text                    as T


dummyBusiness :: T.Text -> NewBusiness
dummyBusiness unique = NewBusiness (GS1CompanyPrefix ("Business" <> unique <> "Prefix"))
                                   ("Business" <> unique <> "Name")
                                   (mockURI unique)


dummyUser :: T.Text -> NewUser
dummyUser unique = NewUser $ "User" <> unique <> "OAuthSub"


generateMultipleUsers :: [T.Text] -> [NewUser]
generateMultipleUsers = fmap dummyUser
