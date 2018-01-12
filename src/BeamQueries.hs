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
  rowID <- dbFunc conn $ ((last $ runInsertReturningList $
        (supplyChainDb ^. _users) $
          insertValues [(User (Auto Nothing) biz first last phone hash email)]) ^. userId)
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
  rowID <- dbFunc conn $ ((last $ runInsertReturningList $
           (supplyChainDb ^. _keys) $
             insertValues [(Key (Auto Nothing) uid n e timestamp 0)]) ^. _keyId) -- TODO = check if 0 is correct here... NOT SURE
  return (fromIntegral rowID :: M.KeyID)

-- TODO = fix. 1 problem is nothing is done with filter value or asset type in objectRowID grabbing data insert
-- 1 other problem is state never used... what is it???
-- epc is a labelEPC
eventCreateObject :: DBConn -> DBFunc -> M.User -> M.NewObject -> IO Event
eventCreateObject conn dbFunc (M.User uid _ _ ) (M.NewObject epc epcisTime timezone location) = do
  objectRowID <- dbFunc conn $ ((last $ runInsertReturningList $ (supplyChainDb ^. _labels) $
                   (case epc of
                     IL il   -> (case il of
                                 GIAI cp sn        -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "GIAI" Nothing)]
                                 SSCC cp sn        -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "SSCC" Nothing)]
                                 SGTIN cp fv ir sn -> insertValues [(Label (Auto Nothing) cp ir sn Nothing "SGTIN" Nothing)]
                                 GRAI cp at sn     -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "GRAI" Nothing)])
                     CL cl q -> (case cl of
                                 LGTIN cp ir lot   -> insertValues [(Label (Auto Nothing) cp ir Nothing Nothing "LGTIN" lot)]
                                 CSGTIN cp fv ir   -> insertValues [(Label (Auto Nothing) cp ir Nothing Nothing "CSGTIN" Nothing)]))) ^. _labelId)

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

eventSign :: (MonadError M.SigError m, MonadIO m) => DBConn -> DBFunc -> M.User -> M.SignedEvent -> m ()
eventSign conn dbFunc (M.User uid _ _ ) (M.SignedEvent eventID keyID (M.Signature signature)) = do
  timestamp <- liftIO getCurrentTime
  rFull <- dbFunc conn $ runSelectReturningList $ select $ do
    allKeys <- all_ (supplyChainDb ^. _keys)
    guard_ (allKeys ^. _keyId ==. keyID)
    pure allKeys
    
  r <- zip ((\e -> e ^. _rsa_n) <$> rFull) ((\e -> e ^. _rsa_e) <$> rFull)

  pubkey <- if length r == 0
    then throwError M.SE_InvalidKeyID
    else
      return $ uncurry M.RSAPublicKey $ head r

  r <- dbFunc conn $ ((\e -> e ^. _jsonEvent) <$> (runSelectReturningList $ select $ do
       allEvents <- all_ (supplyChainDb ^. _events)
       guard_ (allEvents ^. _eventId ==. eventID)
       pure allEvents))

  blob <- case r of
            [Only (x::String)] -> return $ pack x
            _      -> throwError M.SE_InvalidEventID

  checkSignature pubkey blob (M.Signature signature)

  -- TODO = note that there is no hashes table now, so insert into hashes excluded
  -- TODO = userevents is combination of biztransactiontable and _eventCreatedBy field in event table, explore this

  checkReadyForBlockchain conn eventID
  package <- createBlockchainPackage conn eventID
  liftIO $ sendToBlockchain package

-- TODO - convert these below functions, and others in original file Storage.hs

createBlockchainPackage ::  (MonadError M.SigError m, MonadIO m) => DBConn -> EventID -> m C.BlockchainPackage
createBlockchainPackage conn eventID = do
  -- XXX do we want to explicitly check that the hash is signed before assuming the first one is not?
  r <- liftIO $ query_ conn "SELECT hash, signedByUserID FROM Hashes WHERE eventID=? ORDER BY isSigned ASC;" $ Only eventID
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

checkSignature :: (MonadError M.SigError m, MonadIO m) => M.RSAPublicKey -> ByteString.ByteString -> M.Signature -> m ()
checkSignature pubkey blob signature =
  unless (C.verifySignature pubkey blob signature) $
    throwError M.SE_InvalidSignature

-- ready to send to blockchain when all the parties have signed
checkReadyForBlockchain :: (MonadError M.SigError m, MonadIO m) => DBConn -> EventID -> m ()
--checkReadyForBlockchain conn eventID = undefined
checkReadyForBlockchain conn eventID = do
  r <- liftIO $ query_ conn "SELECT COUNT(id) FROM UserEvents WHERE eventID=? AND hasSigned=FALSE;" $ Only eventID
  case r of
    [Only (0::Integer)] -> pure ()
    _ -> throwError M.SE_NeedMoreSignatures