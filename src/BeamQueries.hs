{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BeamQueries where

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

import Control.Lens
import Database.PostgreSQL.Simple.FromField
import Database.Beam.Backend.SQL
import StorageBeam

type DBFunc = MonadBeam syntax be handle m => handle -> m a -> IO a

type DBConn = Connection

insertUser :: DBConn -> DBFunc -> EncryptedPass -> M.NewUser -> IO M.UserID
insertUser conn dbFunc pass (M.NewUser phone email first last biz password) = do
  rowID <-dbFunc conn $ last $ runInsertReturningList $
        (supplyChainDb ^. _users) $
          insertValues [(User (Auto Nothing) biz first last phone hash email)]
   return (fromIntegral rowID :: M.UserID)

newUser :: DBConn -> DBFunc -> M.NewUser -> IO M.UserID
newUser conn dbFunc (M.NewUser phone email first last biz password) = do
   hash <- encryptPassIO' (Pass (pack password))
   return insertUser conn dbFunc hash (M.NewUser phone email first last biz password)


-- offset_ 100 $
-- filter_ (\customer -> ((customerFirstName customer `like_` "Jo%") &&. (customerLastName customer `like_` "S%")) &&.
--                       (addressState (customerAddress customer) ==. just_ "CA" ||. addressState (customerAddress customer) ==. just_ "WA")) $
--         all_ (customer chinookDb)


-- filter_ (\user -> (_emailAddress user) ==. just_ email) $ all_ (user supplyChainDb)

-- selectedUser <- dbFunc conn $ runSelectReturningList $ select $
--   do user <- all_ (supplyChainDb ^. users)
--      guard_ (_emailAddress


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
  rowID <- dbFunc conn $ last $ runInsertReturningList $
           (supplyChainDb ^. _keys) $
             insertValues [(Key (Auto Nothing) uid n e timestamp 0)] -- TODO = check if 0 is correct here... NOT SURE
  return (fromIntegral rowID :: M.KeyID)

-- TODO = fix. 1 problem is nothing is done with filter value or asset type in objectRowID grabbing data insert
-- 1 other problem is state never used... what is it???
-- epc is a labelEPC
eventCreateObject :: DBConn -> DBFunc -> M.User -> M.NewObject -> IO Event
eventCreateObject conn dbFunc (M.User uid _ _ ) (M.NewObject epc epcisTime timezone location) = do
  objectRowID <- dbFunc conn $ last $ runInsertReturningList $ (supplyChainDb ^. _labels) $
                   (case epc of
                     IL il   -> (case il of
                                 GIAI cp sn        -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "GIAI" Nothing)]
                                 SSCC cp sn        -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "SSCC" Nothing)]
                                 SGTIN cp fv ir sn -> insertValues [(Label (Auto Nothing) cp ir sn Nothing "SGTIN" Nothing)]
                                 GRAI cp at sn     -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "GIAI" Nothing)])
                     CL cl q -> (case cl of
                                 LGTIN cp ir lot   -> insertValues [(Label (Auto Nothing) cp ir Nothing Nothing "LGTIN" lot)]
                                 CSGTIN cp fv ir   -> insertValues [(Label (Auto Nothing) cp ir Nothing Nothing "CSGTIN" Nothing)]))

  currentTime <- getCurrentTime
  uuid <- nextRandom
  let
      quantity = ItemCount 3
      what =  ObjectDWhat Add [epc]
      why  =  DWhy (Just CreatingClassInstance) (Just Active)
      when = DWhen epcisTime (Just currentTime) timezone
      eventID = Just $ EventID uuid
      (M.EventLocation readPt bizLoc) = location
      dwhere = DWhere [readPt] [bizLoc] [] []
      event = Event ObjectEventT eventID what when why dwhere
      jsonEvent = encodeEvent event

  dbFunc conn $ DbB.runInsert $
    DbB.insert (supplyChainDb ^. _events) $
      insertValues [ Event (Auto Nothing) eventID epc what why dwhere epcisTime timezone ObjectEventT uid jsonEvent]

  -- TODO = combine rows from bizTransactionTable and _eventCreatedBy field in Event table
  -- haven't added UserEvents insertion equivalent since redundant information and no equivalent
  -- hashes not added yet, but will later for blockchain

  return event

