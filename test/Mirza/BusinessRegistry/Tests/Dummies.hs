
-- | Dummy data used for tests
module Mirza.BusinessRegistry.Tests.Dummies where

import           Data.GS1.EPC                 (GS1CompanyPrefix (..))
import           Mirza.BusinessRegistry.Types as BRT
import           Mirza.Common.Types           as CT

import           Data.Maybe                   (fromJust)

import           Text.Email.Validate          (emailAddress)

dummyNewUser :: BRT.NewUser
dummyNewUser = makeDummyNewUser (fromJust $ emailAddress "fake@gmail.com")

-- | Utility function to make many users on the fly
makeDummyNewUser :: CT.EmailAddress -> BRT.NewUser
makeDummyNewUser userEmail =
    BRT.NewUser "000" userEmail "Bob" "Smith" (GS1CompanyPrefix "blah Ltd") "password"
