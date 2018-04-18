{-# LANGUAGE FlexibleContexts #-}

-- | This module is incomplete as of yet.
-- Functions in the `service` module use the database functions defined here
module BeamQueries where

import           AppConfig                                (AppError (..), AppM,
                                                           runDb)
import           Control.Monad.Except                     (throwError)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Crypto.Scrypt                            as Scrypt
import           Data.GS1.DWhat                           (AggregationDWhat (..),
                                                           DWhat (..),
                                                           InputEPC (..),
                                                           LabelEPC (..),
                                                           ObjectDWhat (..),
                                                           OutputEPC (..),
                                                           TransactionDWhat (..),
                                                           TransformationDWhat (..),
                                                           unParentLabel)
import qualified Data.GS1.Event                           as Ev
import qualified Data.GS1.EventID                         as EvId
import           Data.Maybe                               (fromMaybe)
import qualified Data.Text                                as T
import           Data.Text.Encoding
import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (..),
                                                           constraintViolation)
import           Database.PostgreSQL.Simple.Internal      (SqlError (..))
import           Errors                                   (ServiceError (..))
import           ErrorUtils                               (getSqlErrorCode,
                                                           throwAppError,
                                                           throwBackendError,
                                                           toServerError)
import qualified MigrateUtils                             as MU
import qualified Model                                    as M
import           OpenSSL.PEM                              (writePublicKey)
import           OpenSSL.RSA                              (RSAPubKey)
import           QueryUtils
import qualified StorageBeam                              as SB


{-
-- Sample NewUser JSON
{
  "phoneNumber": "0412",
  "emailAddress": "abc@gmail.com",
  "firstName": "sajid",
  "lastName": "anower",
  "company": "4000001",
  "password": "password"
}
-}

insertUser :: Scrypt.EncryptedPass -> M.NewUser -> AppM M.UserID
insertUser encPass (M.NewUser phone email firstName lastName biz _) = do
  userId <- generatePk
  res <- handleError errHandler $ runDb $ runInsertReturningList (SB._users SB.supplyChainDb) $
    insertValues
      [SB.User userId (SB.BizId  biz) firstName lastName
               phone (Scrypt.getEncryptedPass encPass) email
      ]
  case res of
        [r] -> return $ SB.user_id r
        _   -> throwBackendError res
  where
    errHandler (AppError (DatabaseError e)) = throwAppError $ case constraintViolation e of
        Just (UniqueViolation "users_email_address_key")
          -> EmailExists (toServerError getSqlErrorCode e) email
        _ -> InsertionFail (toServerError (Just . sqlState) e) email
    errHandler e = throwError e
        -- ^ Generic insertion error

-- | Hashes the password of the NewUser and inserts the user into the database
newUser :: M.NewUser -> AppM M.UserID
newUser userInfo@(M.NewUser _ _ _ _ _ password) = do
    hash <- liftIO $ Scrypt.encryptPassIO' (Scrypt.Pass $ encodeUtf8 password)
    insertUser hash userInfo

-- Basic Auth check using Scrypt hashes.
-- TODO: How safe is this to timing attacks? Can we tell which emails are in the
-- system easily?
authCheck :: M.EmailAddress -> M.Password -> AppM (Maybe M.User)
authCheck email password = do
  r <- runDb $ runSelectReturningList $ select $ do
        user <- all_ (SB._users SB.supplyChainDb)
        guard_ (SB.email_address user  ==. val_ email)
        pure user
  case r of
    [user] ->
        if Scrypt.verifyPass' (Scrypt.Pass password) (Scrypt.EncryptedPass $ SB.password_hash user)
          then return $ Just $ userTableToModel user
          else throwAppError $ AuthFailed email
    [] -> throwAppError $ EmailNotFound email
    _  -> throwBackendError r -- multiple elements


-- BELOW = Beam versions of SQL versions from Storage.hs
-- execute conn "INSERT INTO Users (bizID, firstName, lastName, phoneNumber, passwordHash, emailAddress) VALUES (?, ?, ?, ?, ?, ?);" (biz, first, last, phone, getEncryptedPass hash, email)
-- execute conn "INSERT INTO Keys (userID, rsa_n, rsa_e, creationTime) values (?, ?, ?, ?);" (uid, n, e, timestamp)

addPublicKey :: M.User -> RSAPubKey -> AppM M.KeyID
addPublicKey (M.User uid _ _)  rsaPubKey = do
  keyId <- generatePk
  timeStamp <- generateTimeStamp
  keyStr <- liftIO $ writePublicKey rsaPubKey
  r <- runDb $ runInsertReturningList (SB._keys SB.supplyChainDb) $
        insertValues
        [ SB.Key keyId (SB.UserId uid) (T.pack keyStr) timeStamp Nothing
        ]
  case r of
    [rowId] -> return (SB.key_id rowId)
    _       -> throwAppError $ InvalidKeyID keyId


getPublicKey :: M.KeyID -> AppM M.RSAPublicKey
getPublicKey keyId = do
  r <- runDb $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure (SB.pem_str allKeys)
  case r of
    [k] -> return $ M.PEMString $ T.unpack k
    _   -> throwAppError $ InvalidKeyID keyId

getPublicKeyInfo :: M.KeyID -> AppM M.KeyInfo
getPublicKeyInfo keyId = do
  r <- runDb $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure allKeys

  case r of
    [(SB.Key _ (SB.UserId uId) _  creationTime revocationTime)] ->
       return $ M.KeyInfo uId
                (toEPCISTime creationTime)
                (toEPCISTime <$> revocationTime)
    _ -> throwAppError $ InvalidKeyID keyId

-- TODO: Should this return Text or a JSON value?
getEventJSON :: EvId.EventID -> AppM T.Text
getEventJSON eventID = do
  r <- runDb $ runSelectReturningList $ select $ do
    allEvents <- all_ (SB._events SB.supplyChainDb)
    guard_ ((SB.event_id allEvents) ==. val_ (EvId.getEventId eventID))
    pure (SB.json_event allEvents)
  case r of
    [jsonEvent] -> return jsonEvent
    _           -> throwAppError $ InvalidEventID eventID


getUser :: M.EmailAddress -> AppM (Maybe M.User)
getUser email = do
  r <- runDb $ runSelectReturningList $ select $ do
    allUsers <- all_ (SB._users SB.supplyChainDb)
    guard_ (SB.email_address allUsers ==. val_ email)
    pure allUsers
  case r of
    [u] -> return . Just . userTableToModel $ u
    []  -> throwAppError . UserNotFound $ email
    _   -> throwBackendError r

insertObjectEvent :: M.User
                  -> M.ObjectEvent
                  -> AppM Ev.Event
insertObjectEvent
  (M.User userId _ _ )
  (M.ObjectEvent
    foreignEventId
    act
    labelEpcs
    dwhen dwhy dwhere
  ) = do

  let
      eventType = Ev.ObjectEventT
      dwhat =  ObjWhat $ ObjectDWhat act labelEpcs
      event = Ev.Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  transaction $ do
    eventId <- insertEvent userId jsonEvent event
    whatId <- insertDWhat Nothing dwhat eventId
    labelIds <- mapM (insertLabel Nothing whatId) labelEpcs
    _whenId <- insertDWhen dwhen eventId
    _whyId <- insertDWhy dwhy eventId
    insertDWhere dwhere eventId
    insertUserEvent eventId userId userId False Nothing
    mapM_ (insertWhatLabel whatId) labelIds
    mapM_ (insertLabelEvent eventId) labelIds


  return event

insertAggEvent :: M.User
               -> M.AggregationEvent
               -> AppM Ev.Event
insertAggEvent
  (M.User userId _ _ )
  (M.AggregationEvent
    foreignEventId
    act
    mParentLabel
    labelEpcs
    dwhen dwhy dwhere
  ) = do
  let
      eventType = Ev.AggregationEventT
      dwhat =  AggWhat $ AggregationDWhat act mParentLabel labelEpcs
      event = Ev.Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  transaction $ do
    eventId <- insertEvent userId jsonEvent event
    whatId <- insertDWhat Nothing dwhat eventId
    labelIds <- mapM (insertLabel Nothing whatId) labelEpcs
    mapM_ (insertLabel (Just MU.Parent) whatId) ((IL . unParentLabel )<$> mParentLabel)
    _whenId <- insertDWhen dwhen eventId
    _whyId <- insertDWhy dwhy eventId
    insertDWhere dwhere eventId
    insertUserEvent eventId userId userId False Nothing
    mapM_ (insertWhatLabel whatId) labelIds
    mapM_ (insertLabelEvent eventId) labelIds

  -- FIXME: This should return the event as it has been inserted - the user has
  -- no idea what the ID for the transaction is so can't query it later.
  return event

insertTransfEvent :: M.User
                  -> M.TransformationEvent
                  -> AppM Ev.Event
insertTransfEvent
  (M.User userId _ _ )
  (M.TransformationEvent
    foreignEventId
    mTransfId
    inputs
    outputs
    dwhen dwhy dwhere
  ) = do
  let
      eventType = Ev.TransformationEventT
      dwhat =  TransformWhat $ TransformationDWhat mTransfId inputs outputs
      event = Ev.Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  transaction $ do
    eventId <- insertEvent userId jsonEvent event
    whatId <- insertDWhat Nothing dwhat eventId
    inputLabelIds <- mapM (\(InputEPC i) -> insertLabel (Just MU.Input) whatId i) inputs
    outputLabelIds <- mapM (\(OutputEPC o) -> insertLabel (Just MU.Output) whatId o) outputs
    let labelIds = inputLabelIds ++ outputLabelIds
    _whenId <- insertDWhen dwhen eventId
    _whyId <- insertDWhy dwhy eventId
    insertDWhere dwhere eventId
    insertUserEvent eventId userId userId False Nothing
    mapM_ (insertWhatLabel whatId) labelIds
    mapM_ (insertLabelEvent eventId) labelIds


  return event

-- XXX This function is not tested yet.
-- Needs more specifications for implementation.
insertTransactEvent :: M.User
                       -> M.TransactionEvent
                       -> AppM Ev.Event
insertTransactEvent
  (M.User userId _ _ )
  (M.TransactionEvent
    foreignEventId
    act
    mParentLabel
    bizTransactions
    labelEpcs
    _users
    dwhen dwhy dwhere
  ) = do
  let
      eventType = Ev.TransactionEventT
      dwhat =  TransactWhat $ TransactionDWhat act mParentLabel bizTransactions labelEpcs
      event = Ev.Event eventType foreignEventId dwhat dwhen dwhy dwhere
      jsonEvent = encodeEvent event

  transaction $ do
    eventId <- insertEvent userId jsonEvent event
    whatId <- insertDWhat Nothing dwhat eventId
    labelIds <- mapM (insertLabel Nothing whatId) labelEpcs
    mapM_ (insertLabel (Just MU.Parent) whatId) ((IL . unParentLabel )<$> mParentLabel)
    _whenId <- insertDWhen dwhen eventId
    _whyId <- insertDWhy dwhy eventId
    insertDWhere dwhere eventId
    insertUserEvent eventId userId userId False Nothing
    mapM_ (insertWhatLabel whatId) labelIds
    mapM_ (insertLabelEvent eventId) labelIds

  return event


listEvents :: LabelEPC -> AppM [Ev.Event]
listEvents labelEpc = do
  labelId <- findLabelId labelEpc
  fromMaybe (return []) (getEventList <$> labelId)

insertSignature :: EvId.EventID -> M.KeyID -> M.Signature -> M.Digest -> AppM SB.PrimaryKeyType
--insertSignature :: EventID -> KeyID -> Signature -> Digest -> AppM SigID
insertSignature = error "Implement me"

  -- error "not implemented yet"
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

addContact :: M.User -> M.UserID -> AppM Bool
addContact (M.User uid1 _ _) uid2 = do
  pKey <- generatePk
  r <- runDb $ runInsertReturningList (SB._contacts SB.supplyChainDb) $
               insertValues [SB.Contact pKey (SB.UserId uid1) (SB.UserId uid2)]
  verifyContact r uid1 uid2

-- | The current behaviour is, if the users were not contacts in the first
-- place, then the function returns false
-- otherwise, removes the user. Checks that the user has been removed,
-- and returns (not. userExists)
-- @todo Make ContactErrors = NotAContact | DoesntExist | ..
removeContact :: M.User -> M.UserID -> AppM Bool
removeContact (M.User uid1 _ _) uid2 = transaction $ do
  contactExists <- isExistingContact uid1 uid2
  if contactExists
    then do
      runDb $ runDelete $ delete (SB._contacts SB.supplyChainDb)
              (\ contact ->
                SB.contact_user1_id contact ==. val_ (SB.UserId uid1) &&.
                SB.contact_user2_id contact ==. val_ (SB.UserId uid2))
      not <$> isExistingContact uid1 uid2
  else return False

-- | Checks if a pair of userIds are recorded as a contact.
-- __Must be run in a transaction!__
isExistingContact :: M.UserID -> M.UserID -> AppM Bool
isExistingContact uid1 uid2 = do
  r <- runDb $ runSelectReturningList $ select $ do
        contact <- all_ (SB._contacts SB.supplyChainDb)
        guard_ (SB.contact_user1_id contact  ==. (val_ . SB.UserId $ uid1) &&.
                SB.contact_user2_id contact  ==. (val_ . SB.UserId $ uid2))
        pure contact
  verifyContact r uid1 uid2

-- | Simple utility function to check that the users are part of the contact
-- typically used with the result of a query
verifyContact :: (Eq (PrimaryKey SB.UserT f), Monad m) =>
                   [SB.ContactT f] ->
                   C f SB.PrimaryKeyType ->
                   C f SB.PrimaryKeyType ->
                   m Bool
verifyContact [insertedContact] uid1 uid2 = return $
                  (SB.contact_user1_id insertedContact == (SB.UserId uid1)) &&
                  (SB.contact_user2_id insertedContact == (SB.UserId uid2))
verifyContact _ _ _ = return False


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

-- note that use of complex from Data.List.Unique is not efficient
-- no union, so we process making unique in haskell

-- | Lists all the contacts associated with the given user
-- FIXME: Doesn't actually use user, leaks all contacts of all users!
listContacts :: M.User -> AppM [M.User]
listContacts  (M.User _uid _ _) = do
  users <- runDb $ runSelectReturningList $ select $ do
    user <- all_ (SB._users SB.supplyChainDb)
    contact <- all_ (SB._contacts SB.supplyChainDb)
    guard_ (SB.contact_user1_id contact `references_` user)
    pure user
  pure (map userTableToModel users)


  -- r_toGrabUser1 <- runDb $ runSelectReturningList $ select $ do
  --   allUsers <- all_ (SB._users SB.supplyChainDb)
  --   allContacts <- all_ (SB._contacts SB.supplyChainDb)
  --   guard_ (_contactUser2Id allContacts ==. uid &&. _userId allUsers ==. _contactUser1Id allContacts)
  --   pure allUsers
  -- return $ (\l -> (complex l) ^. _1) $ contactUserToUser <$> (r_toGrabUser1 ++ r_toGrabUser2)


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

