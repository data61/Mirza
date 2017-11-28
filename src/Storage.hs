{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Storage where

import Database.SQLite.Simple as Sql
import Database.SQLite.Simple.Types as SqlTypes
import Database.SQLite.Simple.ToField

import Control.Monad.Except

import qualified Model as M
import qualified CryptHash as C
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
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as ByteString
import qualified Data.Text.Lazy as TxtL

import qualified Data.List.NonEmpty as NonEmpty


-- to put in DB, we just convert it to json for now.





-- Users in the system
userTable   =  "CREATE TABLE IF NOT EXISTS Users (id INTEGER PRIMARY KEY AUTOINCREMENT, bizID INTEGER, firstName TEXT NOT NULL, lastName TEXT, phoneNumber TEXT, passwordHash BLOB, emailAddress TEXT);"

-- Public Keys belonging to users
keyTable    =  "CREATE TABLE IF NOT EXISTS Keys (id INTEGER PRIMARY KEY AUTOINCREMENT, userID INTEGER, rsa_n INTEGER, rsa_e INTEGER, creationTime INTEGER, revocationTime INTEGER DEFAULT NULL);"

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
hashTable   =  "CREATE TABLE IF NOT EXISTS Hashes (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID INTEGER NOT NULL, hash BLOB NOT NULL, isSigned INTEGER DEFAULT 0, signedByUserID INTEGER, keyID INTEGER DEFAULT -1,timestamp INTEGER NOT NULL);"

-- This table contains the hash of all the hashes that make up an event. It also has a cross
-- reference ID to the blockchain implementation, so that it can be found later
-- blockChain address is the location/type of the blockchain, and the id is how to
-- find the item on the blockchain
blockChainTable = "CREATE TABLE IF NOT EXISTS BlockchainTable (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID INTEGER NOT NULL, hash BLOB NOT NULL, blockChain address text NOT NULL, blockChainID INTEGER NOT NULL);"

userEventsTable = "CREATE TABLE IF NOT EXISTS UserEvents (id INTEGER PRIMARY KEY AUTOINCREMENT, eventID TEXT NOT NULL, userID INTEGER NOT NULL, hasSigned INTEGER DEFAULT 0,  addedBy INTEGER NOT NULL, signedHash BLOB);"


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
authCheck :: Sql.Connection -> M.EmailAddress -> M.Password -> IO (Maybe M.User)
authCheck conn email password = do
  r <- Sql.query conn "SELECT rowID, firstName, lastName, passwordHash FROM Users WHERE emailAddress = ?;" $ Only $ unpack $ email
  if length r == 0
     then return Nothing
     else do
       let (uid, firstName, lastName, hash) = head r
       if verifyPass' (Pass password) (EncryptedPass hash)
          then return $ Just $ M.User uid firstName lastName
          else return Nothing


-- Add the users public key to the DB
addPublicKey :: Sql.Connection -> M.User -> M.RSAPublicKey-> IO (M.KeyID)
addPublicKey conn (M.User uid _ _)  (M.RSAPublicKey n e) = do
  timestamp <- getCurrentTime
  execute conn "INSERT INTO Keys (userID, rsa_n, rsa_e, creationTime) values (?, ?, ?, ?);" (uid, n, e, timestamp)
  rowID <- lastInsertRowId conn
  return ((fromIntegral rowID) :: M.KeyID)

-- Get a particular public key from the DB
getPublicKey :: (MonadError M.GetPropertyError m, MonadIO m) => Sql.Connection -> M.KeyID -> m M.RSAPublicKey
getPublicKey conn keyID = do
  rs <- liftIO $ Sql.query conn "SELECT rsa_n, rsa_e FROM Keys WHERE keyID = ?;" $ Only keyID
  if length rs == 0
     then throwError M.KE_InvalidKeyID
     else do
       return $ uncurry M.RSAPublicKey $ head rs

-- Get information about a particular public key from the DB
-- This is separate to the getPublicKey function because the former
-- returns binary data. Separate calls for binary data and JSON data are just easier
-- to deal with on both sides of the network.
getPublicKeyInfo :: (MonadError M.GetPropertyError m, MonadIO m) => Sql.Connection -> M.KeyID -> m M.KeyInfo
getPublicKeyInfo conn keyID = do
  r <- liftIO $ Sql.query conn "SELECT UserID, creationTime, revocationTime FROM Keys WHERE keyID = ?;" $ Only keyID
  if length r == 0
     then throwError M.KE_InvalidKeyID
     else do
       let (uid, creationTime, revocationTime) = head r
       return $ M.KeyInfo uid creationTime revocationTime

--getUser :: Sql.Connection -> M.EmailAddress -> IO (Maybe M.User)
--just for debugging atm.
getUser conn email = do
  r <- Sql.query conn "SELECT rowID, firstName, lastName FROM Users WHERE emailAddress = ?;" (Only (email))
  case length r of
    0 -> return Nothing
    _ -> let
      (uid, firstName, lastName) = head r
      in
        return $ Just (M.User uid firstName lastName)

-- Given a user and a new object event, inserts the new object & the new object
-- event into the db and returns the json encoded copy of the event.
-- It also inserts the json event into the db, later used for hashing
eventCreateObject :: Sql.Connection -> M.User -> M.NewObject -> IO Event
eventCreateObject conn (M.User uid _ _ ) (M.NewObject epc epcisTime timezone objectID location) = do
  execute conn "INSERT INTO Objects (objectID, GS1Barcode) VALUES (?, ?);" (objectID, epc)
  objectRowID <- lastInsertRowId conn -- this should probably be part of the uuid too...
  currentTime <- getCurrentTime
  uuid <- nextRandom
  let
      quantity = ItemCount 3
      what =  ObjectDWhat Add [epc]
      why  =  DWhy (Just CreatingClassInstance) (Just Active)
      when = DWhen epcisTime (Just currentTime) timezone
      eventID = EventID uuid
      (M.EventLocation readPt bizLoc) = location
      dwhere = DWhere [readPt] [bizLoc] [] []
      event = mkEvent ObjectEventT eventID what when why dwhere
      jsonEvent = encodeEvent event
  -- insert the event into the events db. Include a json encoded copy, later used for hashing and signing.
  execute conn "INSERT INTO Events (eventID, objectID, what, why, location, timestamp, timezone, eventType, createdBy, jsonEvent) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" (eventID, objectID, what, why, dwhere, epcisTime, timezone, ObjectEventT, uid, jsonEvent)
  -- associate the event with the user. It's not signed yet.
  execute conn "INSERT INTO UserEvents (eventID, userID, hasSigned, addedBy) VALUES (?, ?, ?, ?);" (eventID, uid, False, uid)
  --insert a json representation of it into our table of hashes, it's not signed so it's clear text (not actually a hash).
  execute conn "INSERT INTO Hashes (eventID, hash, isSigned, timestamp) VALUES (?, ?, ?, ?);" (eventID, jsonEvent, False, currentTime)
  return event

-- List the users associated with an event
eventUserList :: Sql.Connection -> M.User -> EventID -> IO [(M.User, Bool)]
eventUserList conn (M.User uid _ _ ) eventID = do
  r <- Sql.query conn "SELECT userID, firstName, lastName, hasSigned FROM UserEvents, Users WHERE eventID=? AND Users.id == UserEvents.userID;" (Only (uid))
  return (map toUserBool r)

  {-
eventAggregateObjects :: Sql.Connection -> M.User -> M.AggregatedObject -> IO Event
eventAggregateObjects conn (M.User uid _ _ ) (M.AggregatedObject objectIDs containerID epcisTime timezone location bizStep disposition) = do
  uuid <- nextRandom
  currentTime <- getCurrentTime
  let
      eventID = EventID uuid
      quantity = QuantityElement (EPCClass objectEPC) (length objectIDs) Nothing
      what = AggregationDWhat Add (Just containerID) objectIDs [quantity] --FIXME
      why = DWy (Just bizStep) (Just disposition)
      when =  DWhen epcisTime (Just currentTime) timezone
      (M.EventLocation readPt bizLoc) = location
      dwhere = DWhere [readPt] [bizLoc] [] []
      event = mkEvent ObjectEventT eventID what when why dwhere
      jsonEvent = encodeEvent $ event
  execute conn "INSERT INTO Events (eventID, objectID, what, why, location, timestamp, timezone, eventType, createdBy, jsonEvent) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);" (eventID, objectID, what, why, dwhere, epcisTime, timezone, ObjectEventT, uid, jsonEvent)
  execute conn "INSERT INTO UserEvents (eventID, userID, hasSigned, addedBy) VALUES (?, ?, ?, ?);" (eventID, uid, False, uid)
  execute conn "INSERT INTO Hashes (eventID, hash, isSigned, timestamp) VALUES (?, ?, ?, ?);" (eventID, jsonEvent, False, currentTime)
  return event
-}



eventHashed :: Sql.Connection -> M.User -> EventID -> IO (Maybe M.HashedEvent)
eventHashed conn _ eventID = do
  -- get an unsigned hash from the db
  r <- Sql.query conn "SELECT hash FROM Hashes WHERE eventID=? AND isSigned=0;" (Only (eventID))
  case (length r) of
    0 -> return Nothing
    _ -> return $ Just (M.HashedEvent eventID (head r))

-- insert it into the hashtable as a signed event & insert it into userevents
eventSign :: (MonadError M.SigError m, MonadIO m) => Sql.Connection -> M.User -> M.SignedEvent -> m ()
eventSign conn (M.User uid _ _ ) (M.SignedEvent eventID keyID (M.Signature signature)) = do
  timestamp <- liftIO getCurrentTime
  -- get publikey key using keyID
  r <- liftIO $ Sql.query conn "SELECT rsa_n, rsa_e FROM Keys WHERE id=?;" $ Only keyID
  pubkey <- if length r == 0
    then throwError M.SE_InvalidKeyID
    else
      return $ uncurry M.RSAPublicKey $ head r
  -- get original json encoded event from db
  r <- liftIO $ Sql.query conn "SELECT jsonEvent FROM Events WHERE eventID=?;" $ Only eventID
  blob <- case r of
            [Only (x::String)] -> return $ pack $ x
            _      -> throwError M.SE_InvalidEventID
  checkSignature pubkey blob (M.Signature signature)
  liftIO $ execute conn "INSERT INTO Hashes (eventID, hash, isSigned, signedByUserID, keyID, timestamp) VALUES (?, ?, ?, ?, ?, ?);" (eventID, signature, True, uid, keyID, timestamp)
  liftIO $ execute conn "UPDATE UserEvents hasSigned=True WHERE eventID=? AND userID=?;" (eventID, uid)
  checkReadyForBlockchain conn eventID
  package <- createBlockchainPackage conn eventID
  liftIO $ sendToBlockchain package

checkSignature :: (MonadError M.SigError m, MonadIO m) => M.RSAPublicKey -> ByteString.ByteString -> M.Signature -> m ()
checkSignature pubkey blob signature =
  if C.verifySignature pubkey blob signature
     then return ()
     else throwError M.SE_InvalidSignature

-- ready to send to blockchain when all the parties have signed
checkReadyForBlockchain :: (MonadError M.SigError m, MonadIO m) => Sql.Connection -> EventID -> m ()
--checkReadyForBlockchain conn eventID = undefined
checkReadyForBlockchain conn eventID = do
  r <- liftIO $ Sql.query conn "SELECT COUNT(id) FROM UserEvents WHERE eventID=? AND hasSigned=FALSE;" $ Only eventID
  case r of
    [Only (0::Integer)] -> pure ()
    _ -> throwError M.SE_NeedMoreSignatures

createBlockchainPackage ::  (MonadError M.SigError m, MonadIO m) => Sql.Connection -> EventID -> m C.BlockchainPackage
createBlockchainPackage conn eventID = do
  -- XXX do we want to explicitly check that the hash is signed before assuming the first one is not?
  r <- liftIO $ Sql.query conn "SELECT hash, signedByUserID FROM Hashes WHERE eventID=? ORDER BY isSigned ASC;" $ Only eventID
  if length r > 2
    then
      let (plainHash, userID) = head r
          signatures = NonEmpty.fromList (map (\(s, u) -> ((M.Signature s), u)) (tail r))
      in
        return $ C.BlockchainPackage (M.EventHash plainHash) signatures
    else throwError M.SE_BlockchainSendFailed

--TODO - Implement me
-- sendToBlockchain ::  (MonadError M.SigError m, MonadIO m) =>  C.BlockchainPackage -> m ()
sendToBlockchain :: Monad m => C.BlockchainPackage -> m ()
sendToBlockchain package = return () -- if it fails, raise SE_SEND_TO_BLOCKCHAIN_FAILED error.

-- Utilities --

toUserBool :: (Integer, String, String, Integer) -> (M.User, Bool)
toUserBool (userID, firstName, lastName, hasSigned) = ((M.User userID firstName lastName), (hasSigned /= 0))
-- json encode the event
-- currently do it automagically, but might what to be
-- more systematic about it so it's easier to replicated. Maybe.
encodeEvent :: Event -> Txt.Text
encodeEvent event = TxtL.toStrict  (encodeToLazyText event)



-- Add contacts to user
addContacts :: Sql.Connection -> M.User -> M.UserID -> IO Bool
addContacts conn (M.User uid1 _ _) uid2 = do
  execute conn "INSERT INTO Contacts (user1, user2) values (?,?);" (uid1, uid2)
  rowID <- lastInsertRowId conn
  return ((fromIntegral rowID) > 0)

-- Remove contacts to user
removeContacts :: Sql.Connection -> M.User -> M.UserID -> IO Bool
removeContacts conn (M.User uid1 _ _) uid2 = do
  execute conn "DELETE FROM Contacts WHERE user1 = ? AND user2 = ?;" (uid1, uid2)
  rowID <- lastInsertRowId conn
  print rowID
  return ((fromIntegral rowID) > 0)

-- list contacts to user
listContacts :: Sql.Connection -> M.User -> IO [(M.User)]
listContacts conn (M.User uid _ _) = do
  rs <- Sql.query conn "SELECT user2, firstName, lastName FROM Contacts, Users WHERE user1 = ? AND user2=Users.id UNION SELECT user1, firstName, lastName FROM Contacts, Users WHERE user2 = ? AND user1=Users.id;" (uid, uid)
  print rs
  return (map toContactUser rs)

-- Search user
userSearch :: Sql.Connection -> M.User -> String -> IO [(M.User)]
userSearch conn (M.User uid _ _) term = do
  rs <- Sql.query conn "SELECT id, firstName, lastName FROM Users WHERE firstName LIKE '%'||?||'%' OR lastName LIKE '%'||?||'%';" (term, term)
  print rs
  return (map toUser rs)


-- Utilities

toContactUser :: (Integer, String, String) -> (M.User)
toContactUser (userID, firstName, lastName) = (M.User userID firstName lastName)

toUser :: (Integer, String, String) -> (M.User)
toUser (userID, firstName, lastName) = (M.User userID firstName lastName)
