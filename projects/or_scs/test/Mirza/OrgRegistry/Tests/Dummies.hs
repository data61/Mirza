
-- | Dummy data used for tests
module Mirza.OrgRegistry.Tests.Dummies where

import           Data.GS1.EPC            (GS1CompanyPrefix (..))
import           Mirza.Common.Utils
import           Mirza.OrgRegistry.Types as ORT

import           Data.Text


dummyOrg :: ORT.NewOrg
dummyOrg = NewOrg (GS1CompanyPrefix "3000001") orgName (mockURI orgName) where
                orgName = "pubKeyTests_orgName"

dummyNewUser :: ORT.NewUser
dummyNewUser = makeDummyNewUser "fake@example.com"

-- | Utility function to make many users on the fly
makeDummyNewUser :: Text -> ORT.NewUser
makeDummyNewUser oAuthSubSuffix = ORT.NewUser ("OAuthSub" <> oAuthSubSuffix)
