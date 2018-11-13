{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions to generate test data using
-- each service's client
module Mirza.BusinessRegistry.Tests.Generate where

-- import           Mirza.BusinessRegistry.Types      as BT

import           Mirza.SupplyChain.Types          as ST

import           Data.GS1.EPC                     (GS1CompanyPrefix (..))

import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)

import qualified Data.ByteString.Char8            as BS

import           Mirza.SupplyChain.Client.Servant as SCSClient

import           Servant.API.BasicAuth            (BasicAuthData (..))

import           Mirza.Common.Tests.Utils         (unsafeMkEmailAddress)
import           Text.Email.Validate              (toByteString)

import           Servant.Client                   (ClientM)

type TestName = String

--TODO: All the functions in this file should be
--converted to functions that can run from the client.
--for example, insertNUsersSCS should create a http client
--and insert the users over the network, just like the client
--tests do.

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


genNUsersSCS :: TestName -> Int -> [ST.NewUser]
genNUsersSCS _ 0        = []
genNUsersSCS testName n = mkNewUserByNumber testName n : genNUsersSCS testName (n - 1)

mkNewUserByNumber :: String -> Int -> ST.NewUser
mkNewUserByNumber testName n =
  let numStr = testName ++ "_" ++ show n
      numT = T.pack numStr
      numBS = BS.pack numStr
  in
  ST.NewUser
  { ST.newUserPhoneNumber = T.append "0400 111 22" numT
  , ST.newUserEmailAddress = unsafeMkEmailAddress $ BS.concat ["abc", numBS, "@example.com"]
  , ST.newUserFirstName = T.append "First: " numT
  , ST.newUserLastName = T.append "Last: " numT
  , ST.newUserCompany = GS1CompanyPrefix $ T.append "671456___" numT
  , ST.newUserPassword = "re4lly$ecret14!"}


insertNUsersSCS :: TestName
                -> Int
                -> [ClientM UserId]
insertNUsersSCS testName n =
  let users = genNUsersSCS testName n
  in
    SCSClient.addUser <$> users

type Firstname = T.Text

-- Insert multiple users into the SCS DB given a
-- list of first names and company prefixes.
insertMultipleUsersSCS  :: TestName
                        -> [Firstname]
                        -> [GS1CompanyPrefix]
                        -> [ClientM UserId]
insertMultipleUsersSCS name fn pfx =
  SCSClient.addUser <$> genMultipleUsersSCS n name fn pfx
  where
    n = min (length fn) (length pfx)


genMultipleUsersSCS :: Int ->  TestName -> [Firstname] ->
    [GS1CompanyPrefix] -> [ST.NewUser]
genMultipleUsersSCS 0 _ _ _ = []
genMultipleUsersSCS _ _ [] _ = []
genMultipleUsersSCS _ _ _ [] = []
genMultipleUsersSCS n testName (f:fx) (p:px) =
  newUser : genMultipleUsersSCS (n-1) testName fx px
  where
    numT = T.pack $ show n
    newUser = ST.NewUser
      { ST.newUserPhoneNumber = T.append "0400 111 22" numT
      , ST.newUserEmailAddress =
          unsafeMkEmailAddress $ BS.concat [encodeUtf8 f, "@example.com"]
      , ST.newUserFirstName = f
      , ST.newUserLastName = T.append "Last: " numT
      , ST.newUserCompany = p
      , ST.newUserPassword = "re4lly$ecret14!"}


-- insertMultipleUsersBR ::
