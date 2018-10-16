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

type TestName = String

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


insertNUsersSCS :: (SCSApp context err, HasScryptParams context)
                => TestName
                -> Int
                -> [AppM context err UserId]
insertNUsersSCS testName n =
  let users = genNUsersSCS testName n
  in
    SCS.addUser <$> users

{-
type Firstname = Text

-- Insert multpile users into the DB given a list of first names and company prefixes.
insertMultipleUsers ::
    TestName ->
    [Firstname] ->
    [GS1CompanyPrefix] ->
    [AppM context err UserId]
insertMultipleUsers name fn pfx = SCS.addUser <$> genMultpleUsers' n name fn pfx
  where
    n = min (length fn) (length pfx)


insertMultipleUsers' :: Int ->  TestName -> [Firstname] -> [GS1CompanyPrefix] -> [UserId]
insertMultipleUsers' 0 _ _ _ = 0
insertMultipleUsers' n testName (f:fx) (p:px) =
  newUser : genMultpleUsers' (n-1) fx px
      where
        numT = T.pack $ show n
        newUser = ST.NewUser
          { ST.newUserPhoneNumber = T.append "0400 111 22" numT
          , ST.newUserEmailAddress =
              unsafeMkEmailAddress $ BS.concat [BS.pack f, "@example.com"]
          , ST.newUserFirstName = f
          , ST.newUserLastName = T.append "Last: " numT
          , ST.newUserCompany = p
          , ST.newUserPassword = "re4lly$ecret14!"}

-}

