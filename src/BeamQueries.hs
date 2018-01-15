{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BeamQueries where

-- import Control.Monad.Except

import qualified Model as M
-- import qualified CryptHash as C
-- import qualified Data.Text as Txt
-- import Control.Monad (unless)
-- import Data.Time.Clock
-- import Data.Maybe (listToMaybe, fromMaybe)

import Crypto.Scrypt

-- import Data.GS1.Event
-- import Data.GS1.EventID
-- import Data.GS1.EPC
-- import Data.GS1.DWhen
-- import Data.GS1.DWhere
-- import Data.GS1.DWhat
-- import Data.GS1.DWhy

-- import Data.UUID.V4

-- import Data.Aeson.Text
-- import Data.ByteString.Char8 (pack, unpack)
-- import qualified Data.ByteString as ByteString
-- import qualified Data.Text.Lazy as TxtL

-- import qualified Data.List.NonEmpty as NonEmpty
import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple as DBConn

import Database.Beam as B
import Database.Beam.Postgres
import Database.Beam.Backend.SQL
import Database.Beam.Backend
import Database.Beam.Backend.SQL.BeamExtensions

import Control.Lens
import Database.PostgreSQL.Simple.FromField
import Database.Beam.Backend.SQL
import StorageBeam
-- import Data.Maybe

import Data.List.Unique

type DBFunc = MonadBeam (syntax::Sql92SanityCheck) (be::BeamBackend) handle (m::MonadIO) => handle -> m a -> IO a

type DBConn = Connection

-- TODO = need to actually define the lenses

insertUser :: DBConn -> DBFunc -> EncryptedPass -> M.NewUser -> IO M.UserID
insertUser conn dbFunc pass (M.NewUser phone email first last biz password) = do
  [rowID] <- dbFunc conn $ runInsertReturningList $
        (_users supplyChainDb) $
          insertValues [(User (Auto Nothing) (BizId . Auto. Just . fromIntegral $ biz) first last phone hash email)]
   return (_userId rowID)

newUser :: DBConn -> DBFunc -> M.NewUser -> IO M.UserID
newUser conn dbFunc (M.NewUser phone email first last biz password) = do
   hash <- encryptPassIO' (Pass (pack password))
   return insertUser conn dbFunc hash (M.NewUser phone email first last biz password)

-- Basic Auth check using Scrypt hashes.
authCheck :: DBConn -> DBFunc -> M.EmailAddress -> M.Password -> IO (Maybe M.User)
authCheck conn dbFunc email password = do
  r <- dbFunc conn $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    guard_ (_emailAddress allUsers  ==. email)
    pure allUsers
  if length r == 0
     then return Nothing
     else do
       let (uid, bizID, firstName, lastName, phoneNumber, hash, emailAddress) = head r
       if verifyPass' (Pass password) (EncryptedPass hash)
          then return $ Just $ M.User uid firstName lastName
          else return Nothing

-- BELOW = Beam versions of SQL versions from Storage.hs

-- execute conn "INSERT INTO Users (bizID, firstName, lastName, phoneNumber, passwordHash, emailAddress) VALUES (?, ?, ?, ?, ?, ?);" (biz, first, last, phone, getEncryptedPass hash, email)
-- execute conn "INSERT INTO Keys (userID, rsa_n, rsa_e, creationTime) values (?, ?, ?, ?);" (uid, n, e, timestamp)

addPublicKey :: DBConn -> DBFunc -> M.User -> M.RSAPublicKey-> IO M.KeyID
addPublicKey conn dbFunc (M.User uid _ _)  (M.RSAPublicKey n e) = do
  timestamp <- getCurrentTime
  [rowID] <- dbFunc conn $ ((runInsertReturningList $
           (_keys supplyChainDb) $
             insertValues [(Key (Auto Nothing) uid n e timestamp 0)])) -- TODO = check if 0 is correct here... NOT SURE
  return (_keyId rowID)

getPublicKey :: (MonadError M.GetPropertyError m, MonadIO m) => DBConn -> DBFunc -> M.KeyID -> m M.RSAPublicKey
getPublicKey conn dbFunc keyID = do
  r <- dbFunc conn $ runSelectReturningList $ select $ do
    allKeys <- all_ (_keys supplyChainDb)
    guard_ (_keyId allKeys ==. keyID)
    pure allKeys
  if length r == 0
     then throwError M.KE_InvalidKeyID
     else do
       let (keyId, uid, rsa_n, rsa_e, creationTime, revocationTime) = head r
       return $ M.RSAPublicKey rsa_n rsa_e

getPublicKeyInfo :: (MonadError M.GetPropertyError m, MonadIO m) => DBConn -> DBFunc -> M.KeyID -> m M.KeyInfo
getPublicKeyInfo conn dbFunc keyID = do
  r <- dbFunc conn $ runSelectReturningList $ select $ do
    allKeys <- all_ (_keys supplyChainDb)
    guard_ (_keyId allKeys ==. keyID)
    pure allKeys
  if length r == 0
     then throwError M.KE_InvalidKeyID
     else do
       let (keyId, uid, rsa_n, rsa_e, creationTime, revocationTime) = head r
       return $ M.KeyInfo uid creationTime revocationTime

getUser :: DBConn -> DBFunc -> M.EmailAddress -> IO (Maybe M.User)
getUser conn dbFunc email = do
  r <- dbFunc conn $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    guard_ (_emailAddress allUsers ==. email)
    pure allUsers
  case length r of
    0 -> return Nothing
    _ -> let
      (uid, bizID, firstName, lastName, phone, hash, emailAddress) = head r
      in
        return $ Just (M.User uid firstName lastName)


-- TODO = fix. 1 problem is nothing is done with filter value or asset type in objectRowID grabbing data insert
-- 1 other problem is state never used... what is it???
-- epc is a labelEPC
eventCreateObject :: DBConn -> DBFunc -> M.User -> M.NewObject -> IO Event
eventCreateObject conn dbFunc (M.User uid _ _ ) (M.NewObject epc epcisTime timezone location) = do
  objectRowID <- dbFunc conn $ runInsert $ (_labels supplyChainDb) $
                   (case epc of
                     IL il   -> (case il of
                                 GIAI cp sn        -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "GIAI" Nothing)]
                                 SSCC cp sn        -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "SSCC" Nothing)]
                                 SGTIN cp fv ir sn -> insertValues [(Label (Auto Nothing) cp ir sn Nothing "SGTIN" Nothing)]
                                 GRAI cp at sn     -> insertValues [(Label (Auto Nothing) cp Nothing sn Nothing "GRAI" Nothing)])
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

  dbFunc conn $ B.runInsert $
    B.insert (_events supplyChainDb) $
      insertValues [ Event (Auto Nothing) eventID epc what why dwhere epcisTime timezone ObjectEventT uid jsonEvent]

  -- TODO = combine rows from bizTransactionTable and _eventCreatedBy field in Event table
  -- haven't added UserEvents insertion equivalent since redundant information and no equivalent
  -- hashes not added yet, but will later for blockchain

  return event

-- TODO = fix... what is definition of hasSigned?
eventUserList :: DBConn -> DBFunc -> M.User -> EventID -> IO [(M.User, Bool)]
eventUserList conn dbFunc (M.User uid _ _ ) eventID = do
  r <- dbFunc conn $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    allEvents <- all_ (_events supplyChainDb)
    guard_ ((_userId allUsers ==. _eventCreatedBy allEvents) &&. (_eventId allEvents ==. eventID) &&. (_eventCreatedBy allEvents ==. uid))
    pure allUsers
  -- TODO = if not creating means false, have to use left join and map null for to false
  return TODO

-- toUserBool :: (Integer, String, String, Integer) -> (M.User, Bool)
-- toUserBool (userID, firstName, lastName, hasSigned) = (M.User userID firstName lastName, hasSigned /= 0)

-- NOT currently relevant since events not currently hashed
-- eventHashed :: DBConn -> DBFunc -> M.User -> EventID -> IO (Maybe M.HashedEvent)
-- eventHashed conn dbFunc _ eventID = do
--   r <- 

eventSign :: (MonadError M.SigError m, MonadIO m) => DBConn -> DBFunc -> M.User -> M.SignedEvent -> m ()
eventSign conn dbFunc (M.User uid _ _ ) (M.SignedEvent eventID keyID (M.Signature signature)) = do
  timestamp <- liftIO getCurrentTime
  rFull <- dbFunc conn $ runSelectReturningList $ select $ do
    allKeys <- all_ (_keys supplyChainDb)
    guard_ (_keyId allKeys ==. keyID)
    pure allKeys
    
  r <- zip ((\e -> _rsa_n e) <$> rFull) ((\e -> _rsa_e e) <$> rFull)

  pubkey <- if length r == 0
    then throwError M.SE_InvalidKeyID
    else
      return $ uncurry M.RSAPublicKey $ head r

  r <- dbFunc conn $ ((\e -> _jsonEvent e) <$> (runSelectReturningList $ select $ do
       allEvents <- all_ (_events supplyChainDb)
       guard_ (_eventId allEvents ==. eventID)
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

addContacts :: DBConn -> DBFunc -> M.User -> M.UserID -> IO Bool
addContacts conn dbFunc (M.User uid1 _ _) uid2 = do
  [rowID] <- dbFunc conn $ runInsertReturningList $
             (_contacts supplyChainDb) $
               insertValues [(Contact (Auto Nothing) uid1 uid2)]
  return (fromIntegral (_contactId rowID) > 0)

-- don't return whether success/failure anymore since no Beam function to help
removeContacts :: DBConn -> DBFunc -> M.User -> M.UserID -> IO ()
removeContacts conn dbFunc (M.User uid1 _ _) uid2 = do
  dbFunc conn $
    runDelete $
    delete (_contacts supplyChainDb)
           (\contact -> _contactUser1Id contact ==. uid1 &&.
                        _contactUser2Id contact ==. uid2)
  return ()



-- TODO - convert these below functions, and others in original file Storage.hs

-- TODO = implement... there is no hash...
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

-- note that use of complex from Data.List.Unique is not efficient
-- no union, so we process making unique in haskell
listContacts :: DBConn -> DBFunc -> M.User -> IO [M.User]
listContacts conn dbFunc (M.User uid _ _) = do
  r_toGrabUser2 <- dbFunc conn $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    allContacts <- all_ (_contacts supplyChainDb)
    guard_ (_contactUser1Id allContacts ==. uid &&. _userId allUsers ==. _contactUser2Id allContacts)
    pure allUsers
  r_toGrabUser1 <- dbFunc conn $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    allContacts <- all_ (_contacts supplyChainDb)
    guard_ (_contactUser2Id allContacts ==. uid &&. _userId allUsers ==. _contactUser1Id allContacts)
    pure allUsers
  return $ (\l -> (complex l) ^. _1) $ contactUserToUser <$> (r_toGrabUser1 ++ r_toGrabUser2)

-- TODO = how to do like, also '%||?||%'
-- below is wrong!
userSearch :: DBConn -> DBFunc -> M.User -> String -> IO [M.User]
userSearch conn dbFunc (M.User uid _ _) term = do
  rs <- dbFunc conn $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    guard_ (_firstName allUsers ==. term ||. lastName ==. term) -- TODO = fix
    pure allUsers
  return (userToUser <$> rs)

userToUser :: (Integer, Integer, String, String, String, String, String) -> M.User
userToUser (userID, _, firstName, lastName, _, _, _, _) = M.User userID firstName lastName