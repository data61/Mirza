{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | This module is incomplete as of yet.
-- Functions in the `service` module use the database functions defined here
module Mirza.SupplyChain.BeamQueries where



import           Mirza.SupplyChain.ErrorUtils             (getSqlErrorCode,
                                                           throwAppError,
                                                           throwBackendError,
                                                           toServerError)
import qualified Mirza.SupplyChain.MigrateUtils           as MU
import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.StorageBeam            as SB
import           Mirza.SupplyChain.Types                  hiding (KeyInfo (..),
                                                           NewUser (..),
                                                           User (userId),
                                                           UserID)
import qualified Mirza.SupplyChain.Types                  as ST

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
import qualified Data.GS1.EventId                         as EvId

import           Control.Monad.Except                     (MonadError,
                                                           throwError)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Crypto.Scrypt                            as Scrypt
import           Data.Bifunctor                           (bimap)
import           Data.Maybe                               (catMaybes)
import qualified Data.Text                                as T
import           Data.Text.Encoding
import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions
import           Database.PostgreSQL.Simple.Errors        (ConstraintViolation (..),
                                                           constraintViolation)
import           Database.PostgreSQL.Simple.Internal      (SqlError (..))
import           OpenSSL.PEM                              (writePublicKey)
import           OpenSSL.RSA                              (RSAPubKey)

import           Control.Lens                             (view, (^?), _2)
{-
-- Sample ST.NewUser JSON
{
  "phoneNumber": "0412",
  "emailAddress": "abc@gmail.com",
  "firstName": "sajid",
  "lastName": "anower",
  "company": "4000001",
  "password": "password"
}
-}

insertUser :: AsServiceError err => Scrypt.EncryptedPass -> ST.NewUser -> DB context err ST.UserID
insertUser encPass (ST.NewUser phone (EmailAddress email) firstName lastName biz _) = do
  userId <- generatePk
  -- TODO: use Database.Beam.Backend.SQL.runReturningOne?
  res <- handleError errHandler $ pg $ runInsertReturningList (SB._users SB.supplyChainDb) $
    insertValues
      [SB.User userId (SB.BizId  biz) firstName lastName
               phone (Scrypt.getEncryptedPass encPass) email
      ]
  case res of
        [r] -> return . ST.UserID . SB.user_id $ r
        -- TODO: Have a proper error response
        _   -> throwBackendError res
  where
    errHandler :: (AsServiceError err, MonadError err m) => err -> m a
    errHandler e = case e ^? _DatabaseError of
      Nothing -> throwError e
      Just sqlErr -> case constraintViolation sqlErr of
        Just (UniqueViolation "users_email_address_key")
          -> throwing _EmailExists (toServerError getSqlErrorCode sqlErr, EmailAddress email)
        _ -> throwing _InsertionFail (toServerError (Just . sqlState) sqlErr, email)

-- | Hashes the password of the ST.NewUser and inserts the user into the database
newUser :: (AsServiceError err, HasScryptParams context) => ST.NewUser -> DB context err ST.UserID
newUser userInfo@(ST.NewUser _ _ _ _ _ password) = do
  params <- view $ _2 . scryptParams
  hash <- liftIO $ Scrypt.encryptPassIO params (Scrypt.Pass $ encodeUtf8 password)
  insertUser hash userInfo

-- Basic Auth check using Scrypt hashes.
-- TODO: How safe is this to timing attacks? Can we tell which emails are in the
-- system easily?
authCheck :: (AsServiceError err, HasScryptParams context) =>  EmailAddress -> Password -> DB context err (Maybe ST.User)
authCheck e@(EmailAddress email) (Password password) = do
  r <- pg $ runSelectReturningList $ select $ do
        user <- all_ (SB._users SB.supplyChainDb)
        guard_ (SB.email_address user  ==. val_ email)
        pure user
  params <- view $ _2 . scryptParams
  case r of
    [user] ->
        case Scrypt.verifyPass params (Scrypt.Pass password)
              (Scrypt.EncryptedPass $ SB.password_hash user)
        of
          (False, _     ) -> throwAppError $ AuthFailed (EmailAddress email)
          (True, Nothing) -> pure $ Just (userTableToModel user)
          (True, Just (Scrypt.EncryptedPass password')) -> do
            _ <- pg $ runUpdate $ update (SB._users SB.supplyChainDb)
                    (\u -> [SB.password_hash u <-. val_ password'])
                    (\u -> SB.user_id u ==. val_ (SB.user_id user))
            pure $ Just (userTableToModel user)
    [] -> throwAppError $ EmailNotFound e
    _  -> throwBackendError r -- multiple elements

addPublicKey :: AsServiceError err =>  ST.User -> RSAPubKey -> DB context err KeyID
addPublicKey (User (ST.UserID uid) _ _)  rsaPubKey = do
  keyId <- generatePk
  timeStamp <- generateTimeStamp
  keyStr <- liftIO $ writePublicKey rsaPubKey
  r <- pg $ runInsertReturningList (SB._keys SB.supplyChainDb) $
        insertValues
        [ SB.Key keyId (SB.UserId uid) (T.pack keyStr) timeStamp Nothing
        ]
  case r of
    [rowId] -> return (KeyID $ SB.key_id rowId)
    _       -> throwing _InvalidKeyID . KeyID $ keyId

getPublicKey :: AsServiceError err =>  KeyID -> DB context err PEM_RSAPubKey
getPublicKey (KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure (SB.pem_str allKeys)
  case r of
    [k] -> return $ PEMString $ T.unpack k
    _   -> throwing _InvalidKeyID . KeyID $ keyId

getPublicKeyInfo :: AsServiceError err => KeyID -> DB context err ST.KeyInfo
getPublicKeyInfo (KeyID keyId) = do
  r <- pg $ runSelectReturningList $ select $ do
    allKeys <- all_ (SB._keys SB.supplyChainDb)
    guard_ (SB.key_id allKeys ==. val_ keyId)
    pure allKeys

  case r of
    [(SB.Key _ (SB.UserId uId) _  creationTime revocationTime)] ->
       return $ ST.KeyInfo (ST.UserID uId)
                (toEPCISTime creationTime)
                (toEPCISTime <$> revocationTime)
    _ -> throwing _InvalidKeyID . KeyID $ keyId

-- TODO: Should this return Text or a JSON value?
getEventJSON :: AsServiceError err => EvId.EventId -> DB context err T.Text
getEventJSON eventID = do
  r <- pg $ runSelectReturningList $ select $ do
    allEvents <- all_ (SB._events SB.supplyChainDb)
    guard_ ((SB.event_id allEvents) ==. val_ (EvId.unEventId eventID))
    pure (SB.json_event allEvents)
  case r of
    [jsonEvent] -> return jsonEvent
    _           -> throwing _InvalidEventID eventID

insertObjectEvent :: ST.User
                  -> ObjectEvent
                  -> DB context err Ev.Event
insertObjectEvent
  (ST.User (ST.UserID userId) _ _ )
  (ObjectEvent
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

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (SB.WhatId whatId)) labelEpcs
  let labelIds = SB.LabelId <$> labelIds'
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (SB.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent (SB.EventId eventId)) labelIds

  return event

insertAggEvent :: ST.User
               -> AggregationEvent
               -> DB context err Ev.Event
insertAggEvent
  (ST.User (ST.UserID userId) _ _ )
  (AggregationEvent
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

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (SB.WhatId whatId)) labelEpcs
  let labelIds = SB.LabelId <$> labelIds'
  mapM_ (insertLabel (Just MU.Parent) (SB.WhatId whatId)) (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (SB.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent (SB.EventId eventId)) labelIds

  -- FIXME: This should return the event as it has been inserted - the user has
  -- no idea what the ID for the transaction is so can't query it later.
  return event

insertTransfEvent :: ST.User
                  -> TransformationEvent
                  -> DB context err Ev.Event
insertTransfEvent
  (ST.User (ST.UserID userId) _ _ )
  (TransformationEvent
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

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  inputLabelIds <- mapM (\(InputEPC i) -> insertLabel (Just MU.Input) (SB.WhatId whatId) i) inputs
  outputLabelIds <- mapM (\(OutputEPC o) -> insertLabel (Just MU.Output) (SB.WhatId whatId) o) outputs
  let labelIds = SB.LabelId <$> (inputLabelIds ++ outputLabelIds)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (SB.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent (SB.EventId eventId)) labelIds


  return event

insertTransactEvent :: ST.User
                    -> TransactionEvent
                    -> DB context err Ev.Event
insertTransactEvent
  (ST.User (ST.UserID userId) _ _ )
  (TransactionEvent
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

  eventId <- insertEvent userId jsonEvent event
  whatId <- insertDWhat Nothing dwhat eventId
  labelIds' <- mapM (insertLabel Nothing (SB.WhatId whatId)) labelEpcs
  let labelIds = SB.LabelId <$> labelIds'
  mapM_ (insertLabel (Just MU.Parent) (SB.WhatId whatId)) (IL . unParentLabel <$> mParentLabel)
  _whenId <- insertDWhen dwhen eventId
  _whyId <- insertDWhy dwhy eventId
  insertDWhere dwhere eventId
  insertUserEvent eventId userId userId False Nothing
  mapM_ (insertWhatLabel (SB.WhatId whatId)) labelIds
  mapM_ (insertLabelEvent (SB.EventId eventId)) labelIds

  return event


listEvents :: AsServiceError err => LabelEPC -> DB context err [Ev.Event]
listEvents labelEpc =
  maybe (return []) (getEventList . SB.LabelId) =<< findLabelId labelEpc

insertSignature :: EvId.EventId -> KeyID -> Signature -> Digest -> DB environmentUnused errorUnused SB.PrimaryKeyType
insertSignature = error "Implement me"

addContact :: ST.User -> ST.UserID -> DB context err Bool
addContact (User (ST.UserID uid1) _ _) (ST.UserID uid2) = do
  pKey <- generatePk
  r <- pg $ runInsertReturningList (SB._contacts SB.supplyChainDb) $
               insertValues [SB.Contact pKey (SB.UserId uid1) (SB.UserId uid2)]
  return $ verifyContact r (SB.UserId uid1) (SB.UserId uid2)

-- | The current behaviour is, if the users were not contacts in the first
-- place, then the function returns false
-- otherwise, removes the user. Checks that the user has been removed,
-- and returns (not. userExists)
-- @todo Make ContactErrors = NotAContact | DoesntExist | ..
removeContact :: ST.User -> ST.UserID -> DB context err Bool
removeContact (User firstId@(ST.UserID uid1) _ _) secondId@(ST.UserID uid2) = do
  contactExists <- isExistingContact firstId secondId
  if contactExists
    then do
      pg $ runDelete $ delete (SB._contacts SB.supplyChainDb)
              (\ contact ->
                SB.contact_user1_id contact ==. val_ (SB.UserId uid1) &&.
                SB.contact_user2_id contact ==. val_ (SB.UserId uid2))
      not <$> isExistingContact firstId secondId
  else return False

-- | Lists all the contacts associated with the given user
listContacts :: ST.User -> DB context err [ST.User]
listContacts  (User (ST.UserID uid) _ _) = do
  userList <- pg $ runSelectReturningList $ select $ do
    user <- all_ (SB._users SB.supplyChainDb)
    contact <- all_ (SB._contacts SB.supplyChainDb)
    guard_ (SB.contact_user1_id contact ==. val_ (SB.UserId uid) &&.
            SB.contact_user2_id contact ==. (SB.UserId $ SB.user_id user))
    pure user
  return $ userTableToModel <$> userList


-- TODO: Write tests
listBusinesses :: DB context err [SB.Business]
listBusinesses = do
  pg $ runSelectReturningList $ select $
      all_ (SB._businesses SB.supplyChainDb)

-- TODO: Write tests
-- Returns the user and whether or not that user had signed the event
eventUserSignedList :: EvId.EventId -> DB context err [(ST.User, Bool)]
eventUserSignedList (EvId.EventId eventId) = do
  usersSignedList <- pg $ runSelectReturningList $ select $ do
    userEvent <- all_ (SB._user_events SB.supplyChainDb)
    user <- all_ (SB._users SB.supplyChainDb)
    guard_ (SB.user_events_event_id userEvent ==. val_ (SB.EventId eventId))
    guard_ (SB.user_events_user_id userEvent `references_` user)
    pure (user, SB.user_events_has_signed userEvent)
  return $ bimap userTableToModel id <$> usersSignedList

eventsByUser :: ST.UserID -> DB context err [Ev.Event]
eventsByUser (ST.UserID userId) = do
  eventList <- pg $ runSelectReturningList $ select $ do
    userEvent <- all_ (SB._user_events SB.supplyChainDb)
    event <- all_ (SB._events SB.supplyChainDb)
    guard_ (SB.user_events_event_id userEvent `references_` event &&.
            SB.user_events_user_id userEvent ==. val_ (SB.UserId userId))
    pure (SB.json_event event)
  return $ catMaybes $ decodeEvent <$> eventList



addUserToEvent :: AsServiceError err => EventOwner -> SigningUser -> EvId.EventId -> DB context err ()
addUserToEvent (EventOwner lUserId@(ST.UserID loggedInUserId))
               (SigningUser (ST.UserID otherUserId))
               evId@(EvId.EventId eventId) = do
  userCreatedEvent <- hasUserCreatedEvent lUserId evId
  if userCreatedEvent
    then insertUserEvent eventId loggedInUserId otherUserId False Nothing
    else throwing _EventPermissionDenied (lUserId, evId)

-- -- TODO - convert these below functions, and others in original file Storage.hs
-- -- TODO = use EventId or EventId ???
-- -- TODO = implement... there is no hash...
-- createBlockchainPackage ::  (MonadError SigError m, MonadIO m) => EventId -> m C.BlockchainPackage


-- createBlockchainPackage eventID = do
--   -- XXX do we want to explicitly check that the hash is signed before assuming the first one is not?
--   r <- liftIO $ query_ conn "SELECT hash, signedByUserID FROM Hashes WHERE eventID=? ORDER BY isSigned ASC;" $ Only eventID
--   if length r > 2
--     then
--       let (plainHash, userID) = head r
--           signatures = NonEmpty.fromList (map (\(s, u) -> ((Signature s), u)) (tail r))
--       in
--         return $ C.BlockchainPackage (EventHash plainHash) signatures
--     else throwError SE_BlockchainSendFailed
-- --TODO - Implement me
-- -- sendToBlockchain ::  (MonadError SigError m, MonadIO m) =>  C.BlockchainPackage -> m ()
-- sendToBlockchain :: Monad m => C.BlockchainPackage -> m ()
-- sendToBlockchain package = return () -- if it fails, raise SE_SEND_TO_BLOCKCHAIN_FAILED error.

-- checkSignature :: (MonadError SigError m, MonadIO m) => PEM_RSAPubKey -> ByteString.ByteString -> Signature -> m ()
-- checkSignature pubkey blob signature =
--   unless (C.verifySignature pubkey blob signature) $
--     throwError SE_InvalidSignature
-- -- TODO = use EventId or EventId
-- -- ready to send to blockchain when all the parties have signed
-- checkReadyForBlockchain :: (MonadError SigError m, MonadIO m) => Connection -> EventId -> m ()
-- checkReadyForBlockchain eventID = do
--   r <- liftIO $ query_ conn "SELECT COUNT(id) FROM UserEvents WHERE eventID=? AND hasSigned=FALSE;" $ Only eventID
--   case r of
--     [Only 0] -> pure ()
--     _ -> throwError SE_NeedMoreSignatures
