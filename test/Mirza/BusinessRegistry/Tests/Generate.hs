{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions to generate test data using
-- each service's client
module Mirza.BusinessRegistry.Tests.Generate where

import           Mirza.BusinessRegistry.Types          as BT

import           Data.GS1.EPC                          (GS1CompanyPrefix (..))

import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)

import qualified Data.ByteString.Char8                 as BS

import           Mirza.BusinessRegistry.Client.Servant as BRClient

import           Servant.API.BasicAuth                 (BasicAuthData (..))

import           Mirza.Common.Tests.Utils              (unsafeMkEmailAddress)
import           Text.Email.Validate                   (toByteString)

import           Servant.Client                        (ClientM)

type TestName = String

firstUser :: NewUser
firstUser = NewUser  { newUserPhoneNumber = "0400 111 222"
  , newUserEmailAddress = unsafeMkEmailAddress "first_honcho@example.com"
  , newUserFirstName = "First"
  , newUserLastName = "User"
  , newUserCompany = GS1CompanyPrefix "100000000"
  , newUserPassword = "re4lly$ecret14!"}

authFirstUser :: BasicAuthData
authFirstUser = BasicAuthData
  (toByteString . newUserEmailAddress $ firstUser)
  (encodeUtf8   . newUserPassword     $ firstUser)


genNUsersBR :: TestName -> Int -> [NewUser]
genNUsersBR _ 0        = []
genNUsersBR testName n = mkNewUserByNumber testName n : genNUsersBR testName (n - 1)

mkNewUserByNumber :: String -> Int -> NewUser
mkNewUserByNumber testName n =
  let numStr = testName ++ "_" ++ show n
      numT = T.pack numStr
      numBS = BS.pack numStr
  in
  NewUser
  { newUserPhoneNumber = T.append "0400 111 22" numT
  , newUserEmailAddress = unsafeMkEmailAddress $ BS.concat ["abc", numBS, "@example.com"]
  , newUserFirstName = T.append "First: " numT
  , newUserLastName = T.append "Last: " numT
  , newUserCompany = GS1CompanyPrefix $ T.append "671456___" numT
  , newUserPassword = "re4lly$ecret14!"}


insertNUsersSCS :: TestName
                -> Int
                -> [ClientM UserId]
insertNUsersSCS testName n =
  let users = genNUsersBR testName n
  in
    BRClient.addUser <$> users

type Firstname = T.Text

-- Insert multiple users into the SCS DB given a
-- list of first names and company prefixes.
insertMultipleUsersBR  :: TestName
                        -> [Firstname]
                        -> [GS1CompanyPrefix]
                        -> [ClientM UserId]
insertMultipleUsersBR name fn pfx =
  BRClient.addUser <$> genMultipleUsersBR n name fn pfx
  where
    n = min (length fn) (length pfx)


genMultipleUsersBR :: Int ->  TestName -> [Firstname] ->
    [GS1CompanyPrefix] -> [NewUser]
genMultipleUsersBR 0 _ _ _ = []
genMultipleUsersBR _ _ [] _ = []
genMultipleUsersBR _ _ _ [] = []
genMultipleUsersBR n testName (f:fx) (p:px) =
  newUser : genMultipleUsersBR (n-1) testName fx px
  where
    numT = T.pack $ show n
    newUser = NewUser
      { newUserPhoneNumber = T.append "0400 111 22" numT
      , newUserEmailAddress =
          unsafeMkEmailAddress $ BS.concat [encodeUtf8 f, "@example.com"]
      , newUserFirstName = f
      , newUserLastName = T.append "Last: " numT
      , newUserCompany = p
      , newUserPassword = "re4lly$ecret14!"}
