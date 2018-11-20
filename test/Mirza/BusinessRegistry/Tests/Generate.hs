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

globalCompanyPrefix :: GS1CompanyPrefix
globalCompanyPrefix = GS1CompanyPrefix "1234567"

globalBusiness :: NewBusiness
globalBusiness = NewBusiness globalCompanyPrefix "Generator Business Ltd."

mkNewUserByNumber :: String -> Int -> NewUser
mkNewUserByNumber testName n =
  let numStr = testName ++ "_" ++ show n
      numT = T.pack numStr
      numBS = BS.pack numStr
  in
  NewUser
  { newUserPhoneNumber = "0400 111 22" <> numT
  , newUserEmailAddress = unsafeMkEmailAddress $ "abc" <> "@example.com"
  , newUserFirstName = "First: " <> numT
  , newUserLastName = "Last: " <> numT
  , newUserCompany = globalCompanyPrefix -- Company prefix is constant
  , newUserPassword = "re4lly$ecret14!" }


insertNUsersBR :: TestName
               -> Int
               -> BasicAuthData
               -> ClientM [UserId]
insertNUsersBR testName n brAuthData =

  let users = genNUsersBR testName n
  in
    sequence $ BRClient.addUser brAuthData <$> users

-- Insert multiple users into the BR DB given a
-- list of first names and company prefixes.
insertMultipleUsersBR :: TestName
                      -> BasicAuthData
                      -> [T.Text]
                      -> [GS1CompanyPrefix]
                      -> ClientM  [UserId]
insertMultipleUsersBR testName brAuthData fn pfx =
  sequence $ BRClient.addUser brAuthData <$> genMultipleUsersBR testName n fn pfx
  where
    n = min (length fn) (length pfx)

genMultipleUsersBR :: TestName
                   -> Int
                   -> [T.Text]
                   -> [GS1CompanyPrefix]
                   -> [NewUser]
genMultipleUsersBR _ 0 _  _ = []
genMultipleUsersBR _ _ [] _ = []
genMultipleUsersBR _ _ _ [] = []
genMultipleUsersBR testName n (f:fx) (p:px) =
  newUser : genMultipleUsersBR testName (n-1) fx px
  where
    numT = T.pack $ show n
    newUser = NewUser
      { newUserPhoneNumber = "0400 111 22" <> numT
      , newUserEmailAddress =
          unsafeMkEmailAddress $ encodeUtf8 f <> "BR" <> "@example.com"
      , newUserFirstName = f
      , newUserLastName = "Last: " <> numT
      , newUserCompany = p
      , newUserPassword = "re4lly$ecret14!" }
