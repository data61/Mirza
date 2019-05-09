
-- | Dummy data used for tests
module Mirza.BusinessRegistry.Tests.Dummies where

import           Data.GS1.EPC                 (GS1CompanyPrefix (..))
import           Mirza.BusinessRegistry.Types as BRT hiding (businessName)
import           Mirza.Common.Utils

import           Data.Text

dummyBusiness :: BRT.NewBusiness
dummyBusiness = NewBusiness (GS1CompanyPrefix "3000001") businessName (mockURI businessName) where
                businessName = "pubKeyTests_businessName"

dummyNewUser :: BRT.NewUser
dummyNewUser = makeDummyNewUser "fake@example.com"

-- | Utility function to make many users on the fly
makeDummyNewUser :: Text -> BRT.NewUser
makeDummyNewUser oAuthSubSuffix = BRT.NewUser ("OAuthSub" <> oAuthSubSuffix)
