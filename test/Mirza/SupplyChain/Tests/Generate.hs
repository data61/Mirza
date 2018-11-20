{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions to generate test data using
-- each service's client
module Mirza.SupplyChain.Tests.Generate where

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
  let numStr = testName <> "_" <> show n
      numT = T.pack numStr
      numBS = BS.pack numStr
  in
  ST.NewUser
  { ST.newUserPhoneNumber ="0400 111 22" <> numT
  , ST.newUserEmailAddress = unsafeMkEmailAddress $ "abc" <> numBS <> "@example.com"
  , ST.newUserFirstName = "First: " <> numT
  , ST.newUserLastName = "Last: "<>  numT
  , ST.newUserCompany = GS1CompanyPrefix $ "671456___" <> numT
  , ST.newUserPassword = "re4lly$ecret14!" }


insertNUsersSCS :: TestName
                -> Int
                -> ClientM [UserId]
insertNUsersSCS testName n =
  let users = genNUsersSCS testName n
  in
    sequence $ SCSClient.addUser <$> users


-- Insert multiple users into the SCS DB given a
-- list of first names and company prefixes.
insertMultipleUsersSCS  :: TestName
                        -> [T.Text]
                        -> [GS1CompanyPrefix]
                        -> ClientM [UserId]
insertMultipleUsersSCS testName firstNames pfx =
  sequence $ SCSClient.addUser <$> genMultipleUsersSCS testName n firstNames pfx
  where
    n = min (length firstNames) (length pfx)


genMultipleUsersSCS :: TestName
                    -> Int
                    -> [T.Text]
                    -> [GS1CompanyPrefix]
                    -> [ST.NewUser]
genMultipleUsersSCS _ 0 _ _ = []
genMultipleUsersSCS _ _ [] _ = []
genMultipleUsersSCS _ _ _ [] = []
genMultipleUsersSCS testName n (f:fx) (p:px) =
  newUser : genMultipleUsersSCS testName (n-1) fx px
  where
    numT = T.pack $ show n
    newUser = ST.NewUser
      { ST.newUserPhoneNumber = "0400 111 22" <> numT
      , ST.newUserEmailAddress =
          unsafeMkEmailAddress $ encodeUtf8 f <> "@example.com"
      , ST.newUserFirstName = f
      , ST.newUserLastName = "Last: " <> numT
      , ST.newUserCompany = p
      , ST.newUserPassword = "re4lly$ecret14!" }
