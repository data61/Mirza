{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-- | Utility functions to generate test data using
-- each service's client
module Mirza.BusinessRegistry.GenerateUtils where

import           Mirza.BusinessRegistry.Types as BT
import           Mirza.Common.Utils           (randomText)

import           Data.GS1.EPC

import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)

import           Text.Email.Validate          (unsafeEmailAddress)

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


generateMultipleUsers :: String
                      -> Int
                      -> [T.Text]
                      -> [GS1CompanyPrefix]
                      -> IO [NewUser]
generateMultipleUsers _ 0 _ _  = pure []
generateMultipleUsers _ _ [] _ = pure []
generateMultipleUsers _ _ _ [] = pure []
generateMultipleUsers testName n (f:fx) (p:px) = do
  newUser <- dummyUser f p
  ((:) newUser) <$> generateMultipleUsers testName (n-1) fx px
