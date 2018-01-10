{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BeamQueries where

-- import Database.SQLite.Simple as Sql
-- import Database.SQLite.Simple.Types as SqlTypes
-- import Database.SQLite.Simple.ToField

import Control.Monad.Except

import qualified Model as M
import qualified CryptHash as C
import qualified Data.Text as Txt
import Control.Monad (unless)
import Data.Time.Clock
import Data.Maybe (listToMaybe, fromMaybe)

import Crypto.Scrypt

import Data.GS1.Event
import Data.GS1.EventID
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy

import Data.UUID.V4

import Data.Aeson.Text
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Lazy as TxtL

import qualified Data.List.NonEmpty as NonEmpty
import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple as DBConn
type DBConn = Connection

insertUser :: DBConn -> DBFunc -> EncryptedPass -> M.NewUser -> IO M.UserID
insertUser conn dbFunc pass (M.NewUser phone email first last biz password) = do
  rowID <-dbFunc conn $ runInsertReturningList $
        insertReturning (supplyChainDb ^. _users) $
          insertValues [(User (Auto Nothing) biz first last phone hash email)]
   return (fromIntegral rowID :: M.UserID)

newUser :: DBConn -> DBFunc -> M.NewUser -> IO M.UserID
newUser conn dbFunc userInfo =
  do
    hash <- encryptPassIO' (Pass (pack password))
    return insertUser conn dbFunc hash userInfo

offset_ 100 $
filter_ (\customer -> ((customerFirstName customer `like_` "Jo%") &&. (customerLastName customer `like_` "S%")) &&.
                      (addressState (customerAddress customer) ==. just_ "CA" ||. addressState (customerAddress customer) ==. just_ "WA")) $
        all_ (customer chinookDb)


filter_ (\user -> (_emailAddress user) ==. just_ email) $ all_ (user supplyChainDb)

selectedUser <- dbFunc conn $ runSelectReturningList $ select $
  do user <- all_ (supplyChainDb ^. users)
     guard_ (_emailAddress


-- Basic Auth check using Scrypt hashes.
authCheck :: DBConn -> DBFunc -> M.EmailAddress -> M.Password -> IO (Maybe M.User)
authCheck conn dbFunc email password = do
  dbFunc conn $ do
    r <- runSelectReturningList $ select theUser
  where
    theUser =
  r <- query_ conn "SELECT rowID, firstName, lastName, passwordHash FROM Users WHERE emailAddress = ?;" $ Only $ unpack email
  if length r == 0
     then return Nothing
     else do
       let (uid, firstName, lastName, hash) = head r
       if verifyPass' (Pass password) (EncryptedPass hash)
          then return $ Just $ M.User uid firstName lastName
          else return Nothing
