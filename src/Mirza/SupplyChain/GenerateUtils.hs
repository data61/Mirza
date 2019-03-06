{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions to generate test data using
-- each service's client
module Mirza.SupplyChain.GenerateUtils where

import           Data.GS1.EPC

import           Mirza.SupplyChain.Types          as ST

import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)

import qualified Data.ByteString.Char8            as BS

import           Mirza.Common.Utils               (unsafeMkEmailAddress)

import           Servant.Client                   (ClientM)

import           Mirza.SupplyChain.Client.Servant as SCSClient

genNUsersSCS :: String -> Int -> [ST.NewUser]
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


insertNUsersSCS :: String
                -> Int
                -> ClientM [UserId]
insertNUsersSCS testName n = traverse (SCSClient.addUser) $ genNUsersSCS testName n


genMultipleUsers:: [(T.Text, GS1CompanyPrefix)] -> [ST.NewUser]
genMultipleUsers = fmap (uncurry newUser)
  where
    newUser f p = ST.NewUser
      { ST.newUserPhoneNumber = "0400 111 223"
      , ST.newUserEmailAddress =
          unsafeMkEmailAddress $ encodeUtf8 f <> "@example.com"
      , ST.newUserFirstName = f
      , ST.newUserLastName = "Last: " <> f
      , ST.newUserCompany = p
      , ST.newUserPassword = "re4lly$ecret14!" }
