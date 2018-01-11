{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Storage where

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

import Database.Beam as DbB
import Database.Beam.Postgres
import Database.Beam.Backend.SQL
import Database.Beam.Backend
import Database.Beam.Backend.SQL.BeamExtensions

type DBFunc = MonadBeam syntax be handle m => handle -> m a -> IO a

type DBConn = Connection

insertUser :: DBConn -> DBFunc -> EncryptedPass -> M.NewUser -> IO M.UserID
insertUser conn dbFunc pass (M.NewUser phone email first last biz password) = do
  rowID <-dbFunc conn $ runInsertReturningList $
        (supplyChainDb ^. _users) $
          insertValues [(User (Auto Nothing) biz first last phone hash email)]
   return (fromIntegral rowID :: M.UserID)

newUser :: DBConn -> DBFunc -> M.NewUser -> IO M.UserID
newUser conn dbFunc (M.NewUser phone email first last biz password) = do
   hash <- encryptPassIO' (Pass (pack password))
   return insertUser conn dbFunc hash (M.NewUser phone email first last biz password)

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
  -- TODO = convert
  r <- query_ conn "SELECT rowID, firstName, lastName, passwordHash FROM Users WHERE emailAddress = ?;" $ Only $ unpack email
  if length r == 0
     then return Nothing
     else do
       let (uid, firstName, lastName, hash) = head r
       if verifyPass' (Pass password) (EncryptedPass hash)
          then return $ Just $ M.User uid firstName lastName
          else return Nothing

-- BELOW = Beam versions of SQL versions from Storage.hs

-- execute conn "INSERT INTO Users (bizID, firstName, lastName, phoneNumber, passwordHash, emailAddress) VALUES (?, ?, ?, ?, ?, ?);" (biz, first, last, phone, getEncryptedPass hash, email)
-- execute conn "INSERT INTO Keys (userID, rsa_n, rsa_e, creationTime) values (?, ?, ?, ?);" (uid, n, e, timestamp)

addPublicKey :: DBConn -> DBFunc -> M.User -> M.RSAPublicKey-> IO M.KeyID
addPublicKey conn dbFunc (M.User uid _ _)  (M.RSAPublicKey n e) = do
  timestamp <- getCurrentTime
  rowID <- dbFunc conn $ runInsertReturningList $
           (supplyChainDb ^. _keys) $
             insertValues [(Key (Auto Nothing) uid n e timestamp 0)] -- TODO = check if 0 is correct here... NOT SURE
  return (fromIntegral rowID :: M.KeyID)

eventCreateObject :: DBConn -> DBFunc -> M.User -> M.NewObject -> IO Event
eventCreateObject conn dbFunc (M.User uid _ _ ) (M.NewObject epc epcisTime timezone location) = do
  objectRowID <- dbFunc conn $runInsertReturningList $
                 (supplyChainDb ^. _)   -- TODO = finish