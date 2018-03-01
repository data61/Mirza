{-# LANGUAGE OverloadedStrings      #-}

-- | This module is incomplete as of yet.
-- Functions in the `service` module use the database functions defined here
module BeamQueries where

import           Data.GS1.Parser.Parser
import qualified Model as M
import qualified StorageBeam as SB
import           Data.ByteString (ByteString)
import           Crypto.Scrypt
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple.Internal (SqlError(..))
import           Database.Beam as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           AppConfig (AppM, runDb, AppError(..))
import           Control.Monad.Except (throwError)
import qualified Data.Text as T
import qualified Data.GS1.EPC as EPC
import           Data.GS1.DWhat (DWhat(..), LabelEPC(..))
import           Data.GS1.DWhy (DWhy(..))
import           Data.GS1.DWhere (DWhere(..), SrcDestLocation)
import           Data.GS1.DWhen (DWhen(..))
import qualified Data.GS1.EventID as EvId
import           Data.GS1.Event (Event(..), EventType(..))
import           Utils (debugLog, toText)
import           QueryUtils
import           Errors (ServiceError(..), ServerError(..))
import           ErrorUtils (throwBackendError, throwAppError, toServerError
                            , defaultToServerError, sqlToServerError
                            , throwUnexpectedDBError)
import           Database.PostgreSQL.Simple.Errors (ConstraintViolation(..)
                                                   , constraintViolation)

{-
{
  "phoneNumber": "0412",
  "emailAddress": "abc@gmail.com",
  "firstName": "sajid",
  "lastName": "anower",
  "company": "4000001",
  "password": "password"
}
-}

insertUser :: EncryptedPass -> M.NewUser -> AppM M.UserID
insertUser encPass (M.NewUser phone email firstName lastName biz _) = do
  userId <- generatePk
  res <- runDb $
            runInsertReturningList (SB._users SB.supplyChainDb) $
            insertValues ([(SB.User userId
            (SB.BizId  biz)
            firstName lastName phone (getEncryptedPass encPass) email)])
  case res of
    Right [r] -> return $ SB.user_id r
    Left e ->
      case constraintViolation e of
        Just (UniqueViolation "users_email_address_key")
            -> throwAppError $ EmailExists (sqlToServerError e) email
        _   -> throwAppError $ InsertionFail (toServerError (Just . sqlState) e) email
    _         -> throwBackendError res

-- | Hashes the password of the NewUser and inserts the user into the database
newUser :: M.NewUser -> AppM M.UserID
newUser userInfo@(M.NewUser _ _ _ _ _ password) = do
    hash <- liftIO $ encryptPassIO' (Pass $ encodeUtf8 password)
    insertUser hash userInfo

-- Basic Auth check using Scrypt hashes.
authCheck :: M.EmailAddress -> M.Password -> AppM (Maybe M.User)
authCheck email password = do
  r <- runDb $
          runSelectReturningList $ select $ do
          user <- all_ (SB._users SB.supplyChainDb)
          guard_ (SB.email_address user  ==. val_ email)
          pure user
  case r of
    Left e -> throwUnexpectedDBError $ sqlToServerError e
    Right [user] -> do
        if verifyPass' (Pass password) (EncryptedPass $ SB.password_hash user)
          then return $ Just $ userTableToModel user
          else throwAppError $ AuthFailed email
    Right [] -> throwAppError $ EmailNotFound email
    _  -> throwBackendError r -- multiple elements


-- BELOW = Beam versions of SQL versions from Storage.hs
-- execute conn "INSERT INTO Users (bizID, firstName, lastName, phoneNumber, passwordHash, emailAddress) VALUES (?, ?, ?, ?, ?, ?);" (biz, first, last, phone, getEncryptedPass hash, email)
-- execute conn "INSERT INTO Keys (userID, rsa_n, rsa_e, creationTime) values (?, ?, ?, ?);" (uid, n, e, timestamp)
addPublicKey :: M.User -> M.RSAPublicKey-> AppM M.KeyID
addPublicKey (M.User uid _ _)  (M.RSAPublicKey rsa_n rsa_e) = do
  keyId <- generatePk
  timeStamp <- generateTimeStamp
  r <- runDb $ runInsertReturningList (SB._keys SB.supplyChainDb) $
               insertValues
               [
                 (
                   SB.Key keyId (SB.UserId uid)
                   (T.pack $ show rsa_n)
                   (T.pack $ show rsa_e)
                   timeStamp
                   Nothing
                 )
               ]
  case r of
    Right [rowId] -> return (SB.key_id rowId)
    Right _       -> throwAppError $ InvalidKeyID keyId
    Left e        -> throwUnexpectedDBError $ sqlToServerError e

getPublicKey :: M.KeyID -> AppM M.RSAPublicKey
getPublicKey keyId = do
  r <- runDb $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure allKeys

  case r of
    Right [k] ->
        return $ M.RSAPublicKey
          (read $ T.unpack $ SB.rsa_n k) (read $ T.unpack $ SB.rsa_e k)
    Right _ -> throwAppError $ InvalidKeyID keyId
    Left e  -> throwUnexpectedDBError $ sqlToServerError e

getPublicKeyInfo :: M.KeyID -> AppM M.KeyInfo
getPublicKeyInfo keyId = do
  r <- runDb $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure allKeys

  case r of
    Right [(SB.Key _ (SB.UserId uId) _ _ creationTime revocationTime)] ->
       return $ M.KeyInfo uId
                (toEPCISTime creationTime)
                (toEPCISTime <$> revocationTime)
    Right _ -> throwAppError $ InvalidKeyID keyId
    Left e  -> throwUnexpectedDBError $ sqlToServerError e

getUser :: M.EmailAddress -> AppM (Maybe M.User)
getUser email = do
  r <- runDb $ runSelectReturningList $ select $ do
    allUsers <- all_ (SB._users SB.supplyChainDb)
    guard_ (SB.email_address allUsers ==. val_ email)
    pure allUsers
  case r of
    Right [u] -> return . Just . userTableToModel $ u
    Right []  -> throwAppError . UserNotFound $ email
    Left e    -> throwUnexpectedDBError $ sqlToServerError e
    _         -> throwBackendError r

insertObjectEvent :: M.User -> Action -> M.ObjectEvent -> AppM SB.PrimaryKeyType
insertObjectEvent
  (M.User userId _ _ )
  act
  (M.ObjectEvent
    foreignEventId
    labelEpcs
    dwhen dwhy dwhere
  ) = do

  currentTime <- generateTimeStamp
  let
      eventType = ObjectEventT
      dwhat =  ObjectDWhat act labelEpcs
      -- dwhere = DWhere [rp] [bizL] [src] [dest]
      -- dwhy  =  DWhy (Just CreatingClassInstance) (Just Active)
      -- dwhen = DWhen epcisTime (Just $ toEPCISTime currentTime) timezone
      event = Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  startTransaction

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing Nothing dwhat eventId
  labelIds <- mapM (insertLabel Nothing whatId) labelEpcs
  whenId <- insertDWhen dwhen eventId
  whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM (insertWhatLabel whatId) labelIds

  endTransaction

  -- TODO = combine rows from bizTransactionTable and _eventCreatedBy field in Event table
  -- haven't added UserEvents insertion equivalent since redundant information and no equivalent
  -- hashes not added yet, but will later for blockchain

  return eventId

-- -- TODO = fix... what is definition of hasSigned?
-- eventUserList :: M.User -> EvId.EventID -> AppM [(M.User, Bool)]
-- eventUserList  (M.User uid _ _ ) eventID = do
--   r <- runDb $ runSelectReturningList $ select $ do
--     allUsers <- all_ (_users supplyChainDb)
--     allEvents <- all_ (_events supplyChainDb)
--     guard_ ((_userId allUsers ==. _eventCreatedBy allEvents) &&. (_eventId allEvents ==. eventID) &&. (_eventCreatedBy allEvents ==. uid))
--     pure allUsers
--   -- TODO = if not creating means false, have to use left join and map null for to false
--   -- return TODO
--   error "TODO"

-- toUserBool :: (SB.PrimaryKeyType, T.Text, T.Text, Integer) -> (M.User, Bool)
-- toUserBool (userID, firstName, lastName, hasSigned) =
--   (M.User userID firstName lastName, hasSigned /= 0)

-- -- NOT currently relevant since events not currently hashed
-- -- eventHashed :: DBFunc -> M.User -> EventID -> IO (Maybe M.HashedEvent)
-- -- eventHashed  dbFunc _ eventID = do
-- --   r <-

-- eventSign :: (MonadError M.SigError m, MonadIO m) => M.User -> M.SignedEvent -> m ()
-- eventSign  (M.User uid _ _ ) (M.SignedEvent eventID keyId (M.Signature signature)) = do
--   timestamp <- liftIO getCurrentTime
--   rFull <- runDb $ runSelectReturningList $ select $ do
--     allKeys <- all_ (_keys supplyChainDb)
--     guard_ (_keyId allKeys ==. keyId)
--     pure allKeys

--   r <- zip ((\e -> _rsa_n e) <$> rFull) ((\e -> _rsa_e e) <$> rFull)

--   pubkey <- if length r == 0
--     then throwError M.SE_InvalidKeyID
--     else
--       return $ uncurry M.RSAPublicKey $ head r

--   r <- runDb $ ((\e -> _jsonEvent e) <$> (runSelectReturningList $ select $ do
--        allEvents <- all_ (_events supplyChainDb)
--        guard_ (_eventId allEvents ==. eventID)
--        pure allEvents))

--   blob <- case r of
--             [Only x] -> return $ pack x
--             _      -> throwError M.SE_InvalidEventID

--   checkSignature pubkey blob (M.Signature signature)

--   -- TODO = note that there is no hashes table now, so insert into hashes excluded
--   -- TODO = userevents is combination of biztransactiontable and _eventCreatedBy field in event table, explore this

--   checkReadyForBlockchain eventID
--   package <- createBlockchainPackage eventID
--   liftIO $ sendToBlockchain package

-- addContacts :: M.User -> M.UserID -> IO Bool
-- addContacts  (M.User uid1 _ _) uid2 = do
--   [rowID] <- runDb $ runInsertReturningList $
--              (_contacts supplyChainDb) $
--                insertValues [(Contact (Auto Nothing) uid1 uid2)]
--   return (fromIntegral (_contactId rowID) > 0)

-- -- don't return whether success/failure anymore since no Beam function to help
-- removeContacts :: M.User -> M.UserID -> IO ()
-- removeContacts  (M.User uid1 _ _) uid2 = do
--   runDb $
--     runDelete $
--     delete (_contacts supplyChainDb)
--            (\contact -> _contactUser1Id contact ==. uid1 &&.
--                         _contactUser2Id contact ==. uid2)
--   return ()



-- -- TODO - convert these below functions, and others in original file Storage.hs

-- -- TODO = use EventId or EventID ???
-- -- TODO = implement... there is no hash...
-- createBlockchainPackage ::  (MonadError M.SigError m, MonadIO m) => EventId -> m C.BlockchainPackage
-- createBlockchainPackage eventID = do
--   -- XXX do we want to explicitly check that the hash is signed before assuming the first one is not?
--   r <- liftIO $ query_ conn "SELECT hash, signedByUserID FROM Hashes WHERE eventID=? ORDER BY isSigned ASC;" $ Only eventID
--   if length r > 2
--     then
--       let (plainHash, userID) = head r
--           signatures = NonEmpty.fromList (map (\(s, u) -> ((M.Signature s), u)) (tail r))
--       in
--         return $ C.BlockchainPackage (M.EventHash plainHash) signatures
--     else throwError M.SE_BlockchainSendFailed

-- --TODO - Implement me
-- -- sendToBlockchain ::  (MonadError M.SigError m, MonadIO m) =>  C.BlockchainPackage -> m ()
-- sendToBlockchain :: Monad m => C.BlockchainPackage -> m ()
-- sendToBlockchain package = return () -- if it fails, raise SE_SEND_TO_BLOCKCHAIN_FAILED error.

-- checkSignature :: (MonadError M.SigError m, MonadIO m) => M.RSAPublicKey -> ByteString.ByteString -> M.Signature -> m ()
-- checkSignature pubkey blob signature =
--   unless (C.verifySignature pubkey blob signature) $
--     throwError M.SE_InvalidSignature

-- -- TODO = use EventId or EventID
-- -- ready to send to blockchain when all the parties have signed
-- checkReadyForBlockchain :: (MonadError M.SigError m, MonadIO m) => Connection -> EventId -> m ()
-- checkReadyForBlockchain eventID = do
--   r <- liftIO $ query_ conn "SELECT COUNT(id) FROM UserEvents WHERE eventID=? AND hasSigned=FALSE;" $ Only eventID
--   case r of
--     [Only 0] -> pure ()
--     _ -> throwError M.SE_NeedMoreSignatures

-- -- note that use of complex from Data.List.Unique is not efficient
-- -- no union, so we process making unique in haskell
-- listContacts :: M.User -> IO [M.User]
-- listContacts  (M.User uid _ _) = do
--   r_toGrabUser2 <- runDb $ runSelectReturningList $ select $ do
--     allUsers <- all_ (_users supplyChainDb)
--     allContacts <- all_ (_contacts supplyChainDb)
--     guard_ (_contactUser1Id allContacts ==. uid &&. _userId allUsers ==. _contactUser2Id allContacts)
--     pure allUsers
--   r_toGrabUser1 <- runDb $ runSelectReturningList $ select $ do
--     allUsers <- all_ (_users supplyChainDb)
--     allContacts <- all_ (_contacts supplyChainDb)
--     guard_ (_contactUser2Id allContacts ==. uid &&. _userId allUsers ==. _contactUser1Id allContacts)
--     pure allUsers
--   return $ (\l -> (complex l) ^. _1) $ contactUserToUser <$> (r_toGrabUser1 ++ r_toGrabUser2)

-- -- TODO = how to do like, also '%||?||%'
-- -- below is wrong!
-- userSearch :: M.User -> String -> IO [M.User]
-- userSearch (M.User uid _ _) term = do
--   rs <- runDb $ runSelectReturningList $ select $ do
--     allUsers <- all_ (_users supplyChainDb)
--     guard_ (_firstName allUsers ==. term ||. lastName ==. term) -- TODO = fix
--     pure allUsers
--   return (userToUser <$> rs)

-- userToUser :: (Integer, Integer, String, String, String, String, String) -> M.User

-- userToUser (userID, _, firstName, lastName, _, _, _, _) = M.User userID firstName lastName

