{-# LANGUAGE FlexibleContexts #-}

-- | Utility functions to generate test data
module Mirza.Common.Tests.Generate where

import           Mirza.BusinessRegistry.Types      as BT

import           Mirza.SupplyChain.Types           as ST

import           Mirza.Common.Tests.Utils          (unsafeMkEmailAddress)

import           Data.GS1.EPC                      (GS1CompanyPrefix (..))

import           Data.Text                         as T

import           Data.ByteString.Char8             as BS

import           Mirza.SupplyChain.Handlers.Users  as SCS

import           Mirza.SupplyChain.Handlers.Common

genNUsersSCS :: Int -> [ST.NewUser]
genNUsersSCS 0 = []
genNUsersSCS n = mkNewUserByNumber n : genNUsersSCS (n - 1)

mkNewUserByNumber :: Int -> ST.NewUser
mkNewUserByNumber n =
  let numStr = show n
      numT = T.pack  numStr
      numBS = BS.pack numStr
  in
  ST.NewUser
  { ST.newUserPhoneNumber = T.append "0400 111 22" numT
  , ST.newUserEmailAddress = unsafeMkEmailAddress $ BS.concat ["abc", numBS, "@example.com"]
  , ST.newUserFirstName = T.append "First: " numT
  , ST.newUserLastName = T.append "Last: " numT
  , ST.newUserCompany = GS1CompanyPrefix $ T.append "671456___" numT
  , ST.newUserPassword = "re4lly$ecret14!"}


insertNUsers :: (SCSApp context err, HasScryptParams context)
             => Int
             -> [AppM context err UserId]
insertNUsers n =
  let users = genNUsersSCS n
  in
    SCS.addUser <$> users
