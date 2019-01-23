{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-- | Utility functions to generate test data using
-- each service's client
module Mirza.BusinessRegistry.GenerateUtils where

import           Mirza.BusinessRegistry.Types          as BT

import           Data.GS1.EPC

import qualified Data.Text                             as T
import           Data.Text.Encoding                    (encodeUtf8)

import           Mirza.BusinessRegistry.Client.Servant as BRClient

import           Servant.API.BasicAuth                 (BasicAuthData (..))

import           Mirza.Common.Utils                    (unsafeMkEmailAddress, randomText)

import           Text.Email.Validate                   (unsafeEmailAddress)

import           Servant.Client                        (ClientM)



globalCompanyPrefix :: GS1CompanyPrefix
globalCompanyPrefix = GS1CompanyPrefix "1234567"


globalBusiness :: NewBusiness
globalBusiness = NewBusiness globalCompanyPrefix "Generator Business Ltd."


dummyBusiness :: T.Text -> IO NewBusiness
dummyBusiness unique = do
  let newBusinessGS1CompanyPrefix = GS1CompanyPrefix ("Business" <> unique <> "Prefix")
  let newBusinessName             = "Business" <> unique <> "Name"
  pure NewBusiness{..}


dummyUser :: T.Text -> GS1CompanyPrefix -> IO NewUser
dummyUser unique business_uid = do
  passwordEntropy <- randomText
  let newUserEmailAddress = unsafeEmailAddress (encodeUtf8 unique) "example.com"
  let newUserPassword     = "User" <> unique <> "Password" <> passwordEntropy
  let newUserCompany      = business_uid
  let newUserFirstName    = "User" <> unique <> "FirstName"
  let newUserLastName     = "User" <> unique <> "LastName"
  let newUserPhoneNumber  = "User" <> unique <> "PhoneNumber"
  pure NewUser{..}

-- Insert multiple users into the BR DB given a
-- list of first names and company prefixes.
insertMultipleUsers :: String
                    -> BasicAuthData
                    -> [T.Text]
                    -> [GS1CompanyPrefix]
                    -> ClientM  [UserId]
insertMultipleUsers testName brAuthData fn pfx =
  traverse (BRClient.addUser brAuthData) (genMultipleUsers testName n fn pfx)
  where
    n = min (length fn) (length pfx)


genMultipleUsers :: String
                 -> Int
                 -> [T.Text]
                 -> [GS1CompanyPrefix]
                 -> [NewUser]
genMultipleUsers _ 0 _  _ = []
genMultipleUsers _ _ [] _ = []
genMultipleUsers _ _ _ [] = []
genMultipleUsers testName n (f:fx) (p:px) =
  newUser : genMultipleUsers testName (n-1) fx px
  where
    numT = T.pack $ show n
    newUser = NewUser
      { newUserPhoneNumber = "0400 111 22" <> numT
      , newUserEmailAddress =
          unsafeMkEmailAddress $ encodeUtf8 f <> "@example.com"
      , newUserFirstName = f
      , newUserLastName = "Last: " <> numT
      , newUserCompany = p
      , newUserPassword = "re4lly$ecret14!" }
