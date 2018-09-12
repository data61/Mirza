
-- | Dummy data used for tests
module Mirza.BusinessRegistry.Tests.Dummies where

import           Data.GS1.EPC                 (GS1CompanyPrefix (..))
import           Mirza.BusinessRegistry.Types as BRT
import           Mirza.Common.Types           as CT

dummyBusiness :: BRT.NewBusiness
dummyBusiness = NewBusiness (GS1CompanyPrefix "3000001") "pubKeyTests_businessName"

dummyNewUser :: BRT.NewUser
dummyNewUser = makeDummyNewUser (EmailAddress "fake@gmail.com")

-- | Utility function to make many users on the fly
makeDummyNewUser :: CT.EmailAddress -> BRT.NewUser
makeDummyNewUser emailAddress =
    BRT.NewUser emailAddress "password" (GS1CompanyPrefix "Company Prefix") "First Name" "Last Name" "Phone Number"
