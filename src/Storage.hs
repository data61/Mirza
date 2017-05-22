{-# LANGUAGE OverloadedStrings #-}
module Storage where

import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes

import qualified Model as M
import qualified Data.Text as Txt
import Data.ByteString.Char8 (pack)

import Data.Time.Clock.POSIX

import Crypto.Scrypt


userTable   =  "CREATE TABLE IF NOT EXISTS Users (id INTEGER PRIMARY KEY AUTOINCREMENT, bizID INTEGER, firstName TEXT NOT NULL, lastName TEXT, phoneNumber TEXT, passwordHash BLOB, emailAddress TEXT);"
keyTable    =  "CREATE TABLE IF NOT EXISTS Keys (id INTEGER PRIMARY KEY AUTOINCREMENT, userID INTEGER, publicKey BLOB, creationTime INTEGER, revocationTime INTEGER DEFAULT NULL);"
bizTable    =  "CREATE TABLE IF NOT EXISTS Business (id INTEGER PRIMARY KEY AUTOINCREMENT, businessName TEXT NOT NULL, location TEXT, businessFunction TEXT);"
contactTable=  "CREATE TABLE IF NOT EXISTS Contacts (id INTEGER PRIMARY KEY AUTOINCREMENT, user1 INTEGER NOT NULL, user2 INTEGER NOT NULL);"
sigTable    =  "CREATE TABLE IF NOT EXISTS Signatures (id INTEGER PRIMARY KEY AUTOINCREMENT, userID INTEGER NOT NULL, keyID INTEGER NOT NULL, timestamp INTEGER NOT NULL);"
hashTable   =  "CREATE TABLE IF NOT EXISTS Hashes (id INTEGER PRIMARY KEY AUTOINCREMENT, signedEventID INTEGER NOT NULL, hashID INTEGER NOT NULL, hash BLOB NOT NULL);"
eventTable  =  "CREATE TABLE IF NOT EXISTS Events (id INTEGER PRIMARY KEY AUTOINCREMENT, what TEXT, why TEXT, location TEXT, timestamp TEXT, eventType INTEGER NOT NULL);"
objectTable = "CREATE TABLE IF NOT EXISTS Objects (id INTEGER PRIMARY KEY AUTOINCREMENT, ObjectID INTEGER NOT NULL, GS1Barcode TEXT NOT NULL);"


getSeconds :: Integral b => IO b
getSeconds = round `fmap` getPOSIXTime

createTables :: Sql.Connection -> IO ()
createTables conn = do
  execute_ conn userTable
  execute_ conn keyTable
  execute_ conn bizTable
  execute_ conn contactTable
  execute_ conn sigTable
  execute_ conn hashTable
  execute_ conn eventTable
  execute_ conn objectTable


--newUser :: Sql.Connection -> M.NewUser -> IO (M.UserID)
newUser conn (M.NewUser phone email first last biz password) = do
  hash <- encryptPassIO' (Pass (pack password))
  execute conn "INSERT INTO Users (bizID, firstName, lastName, phoneNumber, passwordHash, emailAddress) \
    \ VALUES (?, ?, ?, ?)" (biz, first, last, phone, (getEncryptedPass hash), email)
  rowID <- lastInsertRowId conn
  return ((fromIntegral rowID) :: M.UserID)


--authCheck :: Sql.Connection -> M.EmailAddress -> M.Password -> IO (Maybe M.User)
authCheck conn email password = do
  r <- Sql.query conn "SELECT rowID, firstName, lastName, passwordHash FROM Users WHERE emailAddress = ?" (Only (email))
  case (length r) of
    0 -> return Nothing
    _ -> let
      (uid, firstName, lastName, hash) = head r
      in
        do
          case verifyPass' (Pass  password) (EncryptedPass hash) of
            True -> return $ Just (M.User uid firstName lastName)
            False -> return Nothing

addPublicKey :: Sql.Connection -> M.User -> M.BinaryBlob -> IO (M.KeyID)
addPublicKey conn (M.User uid _ _)  (M.BinaryBlob sig) = do
  timestamp <- getSeconds :: IO (Integer)
  execute conn "INSERT INTO Keys (userID, publicKey, creationTime) values (?, ?, ?)" (uid, sig, timestamp)
  rowID <- lastInsertRowId conn
  return ((fromIntegral rowID) :: M.KeyID)


getPublicKey :: Sql.Connection -> M.User -> M.KeyID -> IO (M.BinaryBlob)
getPublicKey = error "not implemented yet"

