{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Contacts
  (
    listContacts
  , addContact
  , removeContact
  , contactsSearch
  , userSearch
  ) where



import           Mirza.SupplyChain.Handlers.Common

import           Mirza.SupplyChain.QueryUtils
import qualified Mirza.SupplyChain.StorageBeam            as SB
import           Mirza.SupplyChain.Types                  hiding (KeyInfo (..),
                                                           NewUser (..),
                                                           User (userId),
                                                           UserID)
import qualified Mirza.SupplyChain.Types                  as ST
import qualified Mirza.SupplyChain.Utils                  as U

import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions



listContacts :: SCSApp context err => ST.User -> AppM context err [ST.User]
listContacts = runDb . listContactsQuery

-- | Lists all the contacts associated with the given user
listContactsQuery :: ST.User -> DB context err [ST.User]
listContactsQuery  (User (ST.UserID uid) _ _) = do
  userList <- pg $ runSelectReturningList $ select $ do
    user <- all_ (SB._users SB.supplyChainDb)
    contact <- all_ (SB._contacts SB.supplyChainDb)
    guard_ (SB.contact_user1_id contact ==. val_ (SB.UserId uid) &&.
            SB.contact_user2_id contact ==. (SB.UserId $ SB.user_id user))
    pure user
  return $ userTableToModel <$> userList



addContact :: SCSApp context err => ST.User -> ST.UserID -> AppM context err Bool
addContact user userId = runDb $ addContactQuery user userId


addContactQuery :: ST.User -> ST.UserID -> DB context err Bool
addContactQuery (User (ST.UserID uid1) _ _) (ST.UserID uid2) = do
  pKey <- generatePk
  r <- pg $ runInsertReturningList (SB._contacts SB.supplyChainDb) $
               insertValues [SB.Contact pKey (SB.UserId uid1) (SB.UserId uid2)]
  return $ verifyContact r (SB.UserId uid1) (SB.UserId uid2)



removeContact :: SCSApp context err => ST.User -> ST.UserID -> AppM context err Bool
removeContact user userId = runDb $ removeContactQuery user userId

-- | The current behaviour is, if the users were not contacts in the first
-- place, then the function returns false
-- otherwise, removes the user. Checks that the user has been removed,
-- and returns (not. userExists)
-- @todo Make ContactErrors = NotAContact | DoesntExist | ..
removeContactQuery :: ST.User -> ST.UserID -> DB context err Bool
removeContactQuery (User firstId@(ST.UserID uid1) _ _) secondId@(ST.UserID uid2) = do
  contactExists <- isExistingContact firstId secondId
  if contactExists
    then do
      pg $ runDelete $ delete (SB._contacts SB.supplyChainDb)
              (\ contact ->
                SB.contact_user1_id contact ==. val_ (SB.UserId uid1) &&.
                SB.contact_user2_id contact ==. val_ (SB.UserId uid2))
      not <$> isExistingContact firstId secondId
  else return False



-- Given a search term, search the users contacts for a user matching
-- that term
-- might want to use reg-ex features of postgres10 here:
-- PSEUDO:
-- SELECT user2, firstName, lastName FROM Contacts, Users WHERE user1 LIKE *term* AND user2=Users.id UNION SELECT user1, firstName, lastName FROM Contacts, Users WHERE user2 = ? AND user1=Users.id;" (uid, uid)
--
contactsSearch :: ST.User -> String -> AppM context err [ST.User]
contactsSearch _user _term = U.notImplemented



userSearch :: ST.User -> String -> AppM context err [ST.User]
-- userSearch user term = liftIO $ Storage.userSearch user term
userSearch _user _term = error "Storage module not implemented"
