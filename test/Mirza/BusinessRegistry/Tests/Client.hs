module Mirza.BusinessRegistry.Tests.Client where

import           Mirza.Common.Tests.Utils              (unsafeMkEmailAddress)

import           Mirza.BusinessRegistry.Tests.Settings (testDbConnStr)
import           Mirza.Common.Test.ServantUtil

import           Control.Concurrent                    (ThreadId)

import           Servant.API.BasicAuth
import           Servant.Client

import           Data.Text.Encoding                    (encodeUtf8)

import           Test.Tasty.Hspec

import           Mirza.BusinessRegistry.Main           (GlobalOptions (..),
                                                        RunServerOptions (..),
                                                        initApplication,
                                                        initBRContext)
import           Mirza.BusinessRegistry.Types

import           Data.GS1.EPC                          (GS1CompanyPrefix (..))


import           Katip                                 (Severity (DebugS))

import           Text.Email.Validate                   (toByteString)

-- === Servant Client tests

userABC :: NewUser
userABC = NewUser
  { newUserPhoneNumber = "0400 111 222"
  , newUserEmailAddress = unsafeMkEmailAddress "abc@example.com"
  , newUserFirstName = "Johnny"
  , newUserLastName = "Smith"
  , newUserCompany = GS1CompanyPrefix "something"
  , newUserPassword = "re4lly$ecret14!"}

authABC :: BasicAuthData
authABC = BasicAuthData
  (toByteString . newUserEmailAddress $ userABC)
  (encodeUtf8   . newUserPassword     $ userABC)

runApp :: IO (ThreadId, BaseUrl)
runApp = do
  let go = GlobalOptions testDbConnStr 14 8 1 DebugS Dev
  ctx <- initBRContext go
  startWaiApp =<< initApplication go (RunServerOptions 8000) ctx

clientSpec :: Spec
clientSpec =
  beforeAll runApp $
  afterAll endWaiApp $ do
    it "Stub" $ \(_,_baseurl) -> do
      pending
