{-# LANGUAGE OverloadedStrings #-}
module Storage where

import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes
import Database.SQLite.Simple.ToField

import qualified Model as M
import qualified Data.Text as Txt

import Data.Time.Clock
import Data.Maybe (listToMaybe, fromMaybe)

import Crypto.Scrypt

import Data.GS1.Event
import Data.GS1.EventID
import Data.GS1.Object
import Data.GS1.EPC
import Data.GS1.DWhen
import Data.GS1.DWhere
import Data.GS1.DWhat
import Data.GS1.DWhy

import Data.UUID.V4

import Data.Aeson.Text
import Data.ByteString.Char8 (pack)
import qualified Data.Text.Lazy as TxtL



-- to put in DB, we just convert it to json for now.





-- Users in the system
userTable   =  "CREATE TABLE IF NOT EXISTS Users (id INTEGER PRIMARY KEY AUTOINCREMENT, bizID INTEGER, firstName TEXT NOT NULL, lastName TEXT, phoneNumber TEXT, passwordHash BLOB, emailAddress TEXT);"

-- Public Keys belonging to users
keyTable    =  "CREATE TABLE IF NOT EXISTS Keys (id INTEGER PRIMARY KEY AUTOINCREMENT, userID INTEGER, publicKey BLOB, creationTime INTEGER, revocationTime INTEGER DEFAULT NULL);"

-- List of businesses
bizTable    =  "CREATE TABLE IF NOT EXISTS Business (id INTEGER PRIMARY KEY AUTOINCREMENT, businessName TEXT NOT NULL, location TEXT, businessFunction TEXT);"

-- List of contact pairs, there's no "friendship" model as yet. You add a contact for
-- the convinience of them appearing on your contact list. This might be changed in the future.
contactTable=  "CREATE TABLE IF NOT EXISTS Contacts (id INTEGER PRIMARY KEY AUTOINCREMENT, user1 INTEGER NOT NULL, user2 INTEGER NOT NULL);"

-- Description of an Event
-- Should we store a JSON copy of the event to ensure we always hash the same thing?
eventTable  =  "CREATE TABLE IF NOT EXISTS Events (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID TEXT, objectID INTEGER NOT NULL, what TEXT, why TEXT, location TEXT, timestamp INTEGER NOT NULL, timezone TEXT, eventType INTEGER NOT NULL, createdBy INTEGER NOT NULL,  jsonEvent TEXT);"

objectTable = "CREATE TABLE IF NOT EXISTS Objects (id INTEGER PRIMARY KEY AUTOINCREMENT, ObjectID TEXT NOT NULL, GS1Barcode TEXT NOT NULL);"

-- A table of hashed events, signed and unsigned.
-- If isSigned is TRUE, then signedByUser must not be null.
hashTable   =  "CREATE TABLE IF NOT EXISTS Hashes (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID INTEGER NOT NULL, hash BLOB NOT NULL, isSigned INTEGER DEFAULT 0, signedByUserID INTEGER, timestamp INTEGER NOT NULL);"

-- This table contains the hash of all the hashes that make up an event. It also has a cross
-- reference ID to the blockchain implementation, so that it can be found later
-- blockChain address is the location/type of the blockchain, and the id is how to
-- find the item on the blockchain
blockChainTable = "CREATE TABLE IF NOT EXISTS BlockchainTable (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID INTEGER NOT NULL, hash BLOB NOT NULL, blockChain address text NOT NULL, blockChainID INTEGER NOT NULL);"

userEventsTable = "CREATE TABLE IF NOT EXISTS UserEvents (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID TEXT NOT NULL, userID INTEGER NOT NULL, hasSigned INTEGER DEFAULT 0, addedBy INTEGER NOT NULL, signedHash BLOB);"


-- Create all the tables above, if they don't exist
createTables :: Sql.Connection -> IO ()
createTables conn = do
  execute_ conn userTable
  execute_ conn keyTable
  execute_ conn bizTable
  execute_ conn contactTable
  execute_ conn hashTable
  execute_ conn eventTable
  execute_ conn objectTable
  execute_ conn blockChainTable
  execute_ conn userEventsTable

{-
   Some testing code, move to test cases later:

import qualified Model as M
import Data.ByteString.Char8 (pack)
import Control.Exception
let nu = M.NewUser "0413045995" "sara@csiro.au" "sara" "falamaki" 3 "foobar"
let bb = M.BinaryBlob (pack "sldkfdssl")
 bracket (open "test.sqlite") (close) (\conn -> do Storage.newUser conn nu)
 bracket (open "test.sqlite") (close) (\conn -> do Storage.authCheck conn "sara@csiro" "foobar")
 bracket (open "test.sqlite") (close) (\conn -> do Storage.addPublicKey conn user bb)
 bracket (open "test.sqlite") (close) (\conn -> Sql.query conn "SELECT rowID, firstName, lastName, passwordHash FROM Users WHERE emailAddress = ?;" (Only ("sara@csiro")))

  -}

-- Create a new user in the database
-- TODO: * do some sort of email/mobile phone verification before enabling their account
--       * add sessionIDs and expiry dates
newUser :: Sql.Connection -> M.NewUser -> IO (M.UserID)
newUser conn (M.NewUser phone email first last biz password) = do
  hash <- encryptPassIO' (Pass (pack password))
  execute conn "INSERT INTO Users (bizID, firstName, lastName, phoneNumber, passwordHash, emailAddress) \
               \ VALUES (?, ?, ?, ?, ?, ?);" (biz, first, last, phone, (getEncryptedPass hash), email)
  rowID <- lastInsertRowId conn
  return ((fromIntegral rowID) :: M.UserID)


-- Basic Auth check using Scrypt hashes.
--authCheck :: Sql.Connection -> M.EmailAddress -> M.Password -> IO (Maybe M.User)
authCheck conn email password = do
  r <- Sql.query conn "SELECT rowID, firstName, lastName, passwordHash FROM Users WHERE emailAddress = ?;" (Only (email))
  case (length r) of
    0 -> return Nothing
    _ -> let
      (uid, firstName, lastName, hash) = head r
      in
        do
          case verifyPass' (Pass  password) (EncryptedPass hash) of
            True -> return $ Just (M.User uid firstName lastName)
            False -> return Nothing

-- Add the users public key to the DB
addPublicKey :: Sql.Connection -> M.User -> M.BinaryBlob -> IO (M.KeyID)
addPublicKey conn (M.User uid _ _)  (M.BinaryBlob sig) = do
  timestamp <- getCurrentTime
  execute conn "INSERT INTO Keys (userID, publicKey, creationTime) values (?, ?, ?);" (uid, sig, timestamp)
  rowID <- lastInsertRowId conn
  return ((fromIntegral rowID) :: M.KeyID)

-- Get a particular public key from the DB
getPublicKey :: Sql.Connection -> M.KeyID -> IO (Maybe M.BinaryBlob)
getPublicKey conn keyID = do
  rs <- Sql.query conn "SELECT publicKey FROM Keys WHERE keyID = ?;" (Only (keyID))
  return $ listToMaybe rs

-- Get information about a particular public key from the DB
-- This is separate to the getPublicKey function because the former
-- returns binary data. Separate calls for binary data and JSON data are just easier
-- to deal with on both sides of the network.
getPublicKeyInfo :: Sql.Connection -> M.KeyID -> IO (Maybe M.KeyInfo)
getPublicKeyInfo conn keyID = do
  r <- Sql.query conn "SELECT UserID, creationTime, revocationTime FROM Keys WHERE keyID = ?;" (Only (keyID))
  case (length r) of
    0 -> return Nothing
    _ -> let
      (uid, creationTime, revocationTime) = head r
      in
        return (Just (M.KeyInfo uid creationTime revocationTime))

--getUser :: Sql.Connection -> M.EmailAddress -> IO (Maybe M.User)
--just for debugging atm.
getUser conn email = do
  r <- Sql.query conn "SELECT rowID, firstName, lastName FROM Users WHERE emailAddress = ?;" (Only (email))
  case (length r) of
    0 -> return Nothing
    _ -> let
      (uid, firstName, lastName) = head r
      in
        do
          return $ Just (M.User uid firstName lastName)

-- Given a user and a new object event, inserts the new object & the new object
-- event into the db and returns the json encoded copy of the event.
-- It also inserts the json event into the db, later used for hashing
eventCreateObject :: Sql.Connection -> M.User -> M.NewObject -> IO Event
eventCreateObject conn (M.User uid _ _ ) (M.NewObject epc epcisTime timezone objectID location) = do
  execute conn "INSERT INTO Objects (objectID, GS1Barcode) VALUES (?, ?);" (objectID, epc)
  objectRowID <- lastInsertRowId conn
  currentTime <- getCurrentTime
  uuid <- nextRandom
  let
      quantity = QuantityElement (EPCClass "FIXME") 1.0 Nothing
      what =  ObjectDWhat Add [epc] [quantity]
      why  =  DWhy (Just CreatingClassInstance) (Just Active)
      when = DWhen epcisTime (Just currentTime) timezone
      eventID = EventID uuid
      (M.EventLocation readPt bizLoc src dest) = location
      dwhere = DWhere [readPt] [bizLoc] [src] [dest]
      event = mkEvent ObjectEventT eventID what when why dwhere
      jsonEvent = encodeEvent $ event
  -- insert the event into the events db. Include a json encoded copy, later used for hashing and signing.
  execute conn "INSERT INTO Events (eventID, objectID, what, why, location, timestamp, timezone, eventType, createdBy, jsonEvent) VALUES (?, ?, ?, ?, ?, ?, ?, ?);" (eventID, objectID, what, why, dwhere, epcisTime, timezone, ObjectEventT, uid, jsonEvent)
  -- associate the event with the user. It's not signed yet.
  execute conn "INSERT INTO UserEvents (eventID, userID, hasSigned, addedBy) VALUES (?, ?, ?, ?);" (eventID, uid, False, uid)
  return event

-- List the users associated with an event
eventUserList :: Sql.Connection -> M.User -> EventID -> IO [(M.User, Bool)]
eventUserList conn (M.User uid _ _ ) eventID = do
  r <- Sql.query conn "SELECT userID, firstName, lastName, hasSigned FROM UserEvents, Users WHERE eventID=? AND Users.id == UserEvents.userID;" (Only (uid))
  return (map toUserBool r)

toUserBool :: (Integer, String, String, Integer) -> (M.User, Bool)
toUserBool (userID, firstName, lastName, hasSigned) = ((M.User userID firstName lastName), (hasSigned /= 0))

-- json encode the event
-- currently do it automagically, but might what to be
-- more systematic about it so it's easier to replicated. Maybe.
encodeEvent :: Event -> Txt.Text
encodeEvent event = TxtL.toStrict  (encodeToLazyText event)



