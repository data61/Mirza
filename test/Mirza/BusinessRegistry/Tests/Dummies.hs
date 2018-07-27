
-- | Sample data types. Serves as a good example of the types defined
-- in GS1Combinators
module Mirza.BusinessRegistry.Tests.Dummies where

-- import           Mirza.SupplyChain.Types hiding (InProgress, NewUser (..))
-- import qualified Mirza.SupplyChain.Types as ST

import           Data.GS1.EPC                 (GS1CompanyPrefix (..))
import           Mirza.BusinessRegistry.Types as BRT
import           Mirza.Common.Types           as CT

-- add function to generate and take dummyLabelEpc

-- General Utils
dummyNewUser :: BRT.NewUser
dummyNewUser = makeDummyNewUser (EmailAddress "fake@gmail.com")

-- | Utility function to make many users on the fly
makeDummyNewUser :: CT.EmailAddress -> BRT.NewUser
makeDummyNewUser emailAddress =
    BRT.NewUser "000" emailAddress "Bob" "Smith" (GS1CompanyPrefix "blah Ltd") "password"
