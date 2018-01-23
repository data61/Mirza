module BeamQueries where

{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TemplateHaskell, GADTs #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

import qualified Model as M
import qualified CryptHash as C
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.ByteString as ByteString

import           Crypto.Scrypt
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple
import           Database.Beam as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Control.Monad.Error (MonadError)
import           StorageBeam as SB
import           AppConfig (AppM, runDb)
import           Data.GS1.EPC
import           Data.GS1.DWhat
import           Data.Time.Clock (getCurrentTime)
import           Control.Monad.Except (throwError)

insertUser :: EncryptedPass -> M.NewUser -> AppM M.UserID
insertUser pass (M.NewUser phone email firstName lastName biz password) = do
  [insertedUserList] <- runDb $ runInsertReturningList (_users supplyChainDb) $
                    insertValues ([(User (Auto Nothing)
                    (BizId . Auto $ Just biz)--(BizId biz)
                    firstName lastName phone password email)]::[SB.User])
                    -- (BizId . Auto. Just . fromIntegral $ biz) firstName lastName phone password email)]
  return (user_id insertedUserList)

-- |
newUser :: M.NewUser -> AppM M.UserID
newUser userInfo@(M.NewUser _ _ _ _ _ password) = do
    hash <- liftIO $ encryptPassIO' (Pass $ encodeUtf8 password)
    insertUser hash userInfo

userTable2Model :: SB.User -> M.User
userTable2Model = error "not implemented yet"

-- Basic Auth check using Scrypt hashes.
authCheck :: M.EmailAddress -> M.Password -> AppM (Maybe M.User)
authCheck email password = do
  r <- runDb $ runSelectReturningList $ select $ do
        user <- all_ (_users supplyChainDb)
        guard_ (email_address user  ==. val_ email)
        pure user
  case r of
    [user] -> do
        if verifyPass' (Pass password) (EncryptedPass $ encodeUtf8 $ password_hash user)
          then return $ Just $ userTable2Model user
          else return Nothing
    _ -> return Nothing -- null list or multiple elements

-- BELOW = Beam versions of SQL versions from Storage.hs
-- execute conn "INSERT INTO Users (bizID, firstName, lastName, phoneNumber, passwordHash, emailAddress) VALUES (?, ?, ?, ?, ?, ?);" (biz, first, last, phone, getEncryptedPass hash, email)
-- execute conn "INSERT INTO Keys (userID, rsa_n, rsa_e, creationTime) values (?, ?, ?, ?);" (uid, n, e, timestamp)
addPublicKey :: M.User -> M.RSAPublicKey-> AppM M.KeyID
addPublicKey (M.User uid _ _)  (M.RSAPublicKey n e) = do
  timestamp <- liftIO getCurrentTime
  [rowID] <- runDb $ runInsertReturningList (_keys supplyChainDb) $
             insertValues [(Key (Auto Nothing) uid n e timestamp 0)] -- TODO = check if 0 is correct here... NOT SURE
  return (KeyId rowID)

getPublicKey :: (MonadError M.GetPropertyError m, MonadIO m) => M.KeyID -> m M.RSAPublicKey
getPublicKey keyID = do
  r <- runDb $ runSelectReturningList $ select $ do
    allKeys <- all_ (_keys supplyChainDb)
    guard_ (_keyId allKeys ==. keyID)
    pure allKeys
  if length r == 0
     then throwError M.KE_InvalidKeyID
     else do
       let (keyId, uid, rsa_n, rsa_e, creationTime, revocationTime) = head r
       return $ M.RSAPublicKey rsa_n rsa_e

getPublicKeyInfo :: (MonadError M.GetPropertyError m, MonadIO m) => M.KeyID -> m M.KeyInfo
getPublicKeyInfo keyID = do
  r <- runDb $ runSelectReturningList $ select $ do
    allKeys <- all_ (_keys supplyChainDb)
    guard_ (_keyId allKeys ==. keyID)
    pure allKeys
  if length r == 0
     then throwError M.KE_InvalidKeyID
     else do
       let (keyId, uid, rsa_n, rsa_e, creationTime, revocationTime) = head r
       return $ M.KeyInfo uid creationTime revocationTime

getUser :: M.EmailAddress -> AppM (Maybe M.User)
getUser  email = do
  r <- runDb $ runSelectReturningList $ select $ do
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
eventCreateObject :: M.User -> M.NewObject -> AppM Event
eventCreateObject  (M.User uid _ _ ) (M.NewObject epc epcisTime timezone location) = do
  objectRowID <- runDb $ B.runInsert $ (_labels supplyChainDb) $
                   (case epc of
                     IL il   -> (case il of
                                 GIAI cp sn        -> insertValues [Label (Auto Nothing) cp Nothing sn Nothing "GIAI" Nothing]
                                 SSCC cp sn        -> insertValues [Label (Auto Nothing) cp Nothing sn Nothing "SSCC" Nothing]
                                 SGTIN cp fv ir sn -> insertValues [Label (Auto Nothing) cp ir sn Nothing "SGTIN" Nothing]
                                 GRAI cp at sn     -> insertValues [Label (Auto Nothing) cp Nothing sn Nothing "GRAI" Nothing])
                     CL cl q -> (case cl of
                                 LGTIN cp ir lot   -> insertValues [Label (Auto Nothing) cp ir Nothing Nothing "LGTIN" lot]
                                 CSGTIN cp fv ir   -> insertValues [Label (Auto Nothing) cp ir Nothing Nothing "CSGTIN" Nothing]))

  currentTime <- liftIO getCurrentTime
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

  runDb $ B.runInsert $
    B.insert (_events supplyChainDb) $
      insertValues [ Event (Auto Nothing) eventID epc what why dwhere epcisTime timezone ObjectEventT uid jsonEvent]

  -- TODO = combine rows from bizTransactionTable and _eventCreatedBy field in Event table
  -- haven't added UserEvents insertion equivalent since redundant information and no equivalent
  -- hashes not added yet, but will later for blockchain

  return event

-- TODO = use EventId or EventID ???
-- TODO = fix... what is definition of hasSigned?
eventUserList :: M.User -> EventId -> AppM [(M.User, Bool)]
eventUserList  (M.User uid _ _ ) eventID = do
  r <- runDb $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    allEvents <- all_ (_events supplyChainDb)
    guard_ ((_userId allUsers ==. _eventCreatedBy allEvents) &&. (_eventId allEvents ==. eventID) &&. (_eventCreatedBy allEvents ==. uid))
    pure allUsers
  -- TODO = if not creating means false, have to use left join and map null for to false
  -- return TODO
  error "TODO"

-- toUserBool :: (Integer, String, String, Integer) -> (M.User, Bool)
-- toUserBool (userID, firstName, lastName, hasSigned) = (M.User userID firstName lastName, hasSigned /= 0)

-- NOT currently relevant since events not currently hashed
-- eventHashed :: DBFunc -> M.User -> EventID -> IO (Maybe M.HashedEvent)
-- eventHashed  dbFunc _ eventID = do
--   r <- 

eventSign :: (MonadError M.SigError m, MonadIO m) => M.User -> M.SignedEvent -> m ()
eventSign  (M.User uid _ _ ) (M.SignedEvent eventID keyID (M.Signature signature)) = do
  timestamp <- liftIO getCurrentTime
  rFull <- runDb $ runSelectReturningList $ select $ do
    allKeys <- all_ (_keys supplyChainDb)
    guard_ (_keyId allKeys ==. keyID)
    pure allKeys
    
  r <- zip ((\e -> _rsa_n e) <$> rFull) ((\e -> _rsa_e e) <$> rFull)

  pubkey <- if length r == 0
    then throwError M.SE_InvalidKeyID
    else
      return $ uncurry M.RSAPublicKey $ head r

  r <- runDb $ ((\e -> _jsonEvent e) <$> (runSelectReturningList $ select $ do
       allEvents <- all_ (_events supplyChainDb)
       guard_ (_eventId allEvents ==. eventID)
       pure allEvents))

  blob <- case r of
            [Only x] -> return $ pack x
            _      -> throwError M.SE_InvalidEventID

  checkSignature pubkey blob (M.Signature signature)

  -- TODO = note that there is no hashes table now, so insert into hashes excluded
  -- TODO = userevents is combination of biztransactiontable and _eventCreatedBy field in event table, explore this

  checkReadyForBlockchain conn eventID
  package <- createBlockchainPackage conn eventID
  liftIO $ sendToBlockchain package

addContacts :: M.User -> M.UserID -> IO Bool
addContacts  (M.User uid1 _ _) uid2 = do
  [rowID] <- runDb $ runInsertReturningList $
             (_contacts supplyChainDb) $
               insertValues [(Contact (Auto Nothing) uid1 uid2)]
  return (fromIntegral (_contactId rowID) > 0)

-- don't return whether success/failure anymore since no Beam function to help
removeContacts :: M.User -> M.UserID -> IO ()
removeContacts  (M.User uid1 _ _) uid2 = do
  runDb $
    runDelete $
    delete (_contacts supplyChainDb)
           (\contact -> _contactUser1Id contact ==. uid1 &&.
                        _contactUser2Id contact ==. uid2)
  return ()



-- TODO - convert these below functions, and others in original file Storage.hs

-- TODO = use EventId or EventID ???
-- TODO = implement... there is no hash...
createBlockchainPackage ::  (MonadError M.SigError m, MonadIO m) => EventId -> m C.BlockchainPackage
createBlockchainPackage  eventID = do
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

-- TODO = use EventId or EventID
-- ready to send to blockchain when all the parties have signed
checkReadyForBlockchain :: (MonadError M.SigError m, MonadIO m) => Connection -> EventId -> m ()
checkReadyForBlockchain eventID = do
  r <- liftIO $ query_ conn "SELECT COUNT(id) FROM UserEvents WHERE eventID=? AND hasSigned=FALSE;" $ Only eventID
  case r of
    [Only 0] -> pure ()
    _ -> throwError M.SE_NeedMoreSignatures

-- note that use of complex from Data.List.Unique is not efficient
-- no union, so we process making unique in haskell
listContacts :: M.User -> IO [M.User]
listContacts  (M.User uid _ _) = do
  r_toGrabUser2 <- runDb $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    allContacts <- all_ (_contacts supplyChainDb)
    guard_ (_contactUser1Id allContacts ==. uid &&. _userId allUsers ==. _contactUser2Id allContacts)
    pure allUsers
  r_toGrabUser1 <- runDb $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    allContacts <- all_ (_contacts supplyChainDb)
    guard_ (_contactUser2Id allContacts ==. uid &&. _userId allUsers ==. _contactUser1Id allContacts)
    pure allUsers
  return $ (\l -> (complex l) ^. _1) $ contactUserToUser <$> (r_toGrabUser1 ++ r_toGrabUser2)

-- TODO = how to do like, also '%||?||%'
-- below is wrong!
userSearch :: M.User -> String -> IO [M.User]
userSearch (M.User uid _ _) term = do
  rs <- runDb $ runSelectReturningList $ select $ do
    allUsers <- all_ (_users supplyChainDb)
    guard_ (_firstName allUsers ==. term ||. lastName ==. term) -- TODO = fix
    pure allUsers
  return (userToUser <$> rs)

userToUser :: (Integer, Integer, String, String, String, String, String) -> M.User

userToUser (userID, _, firstName, lastName, _, _, _, _) = M.User userID firstName lastName