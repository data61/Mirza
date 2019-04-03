{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Mirza.SupplyChain.Handlers.Contacts
  (
    listContacts
  , addContact
  , removeContact
  , isExistingContact
  , userSearch
  ) where

import           Data.GS1.EPC                             (GS1CompanyPrefix)
import qualified Mirza.Common.Utils                       as U
import           Mirza.SupplyChain.Database.Schema        as Schema
import           Mirza.SupplyChain.QueryUtils
import           Mirza.SupplyChain.Types                  hiding (NewUser (..),
                                                           User (userId),
                                                           UserId)
import qualified Mirza.SupplyChain.Types                  as ST

import           Data.Foldable                            (for_)
import           Data.Maybe                               (isNothing)
import           Data.Text                                (Text)
import           Database.Beam                            as B
import           Database.Beam.Backend.SQL.BeamExtensions



listContacts  :: (Member context '[HasDB],
                  Member err     '[AsSqlError])
              => ST.User -> AppM context err [ST.User]
listContacts = runDb . listContactsQuery

-- | Lists all the contacts associated with the given user
listContactsQuery :: ST.User -> DB context err [ST.User]
listContactsQuery  (ST.User (ST.UserId uid) _ _) = do
  userList <- pg $ runSelectReturningList $ select $ do
    user <- all_ (Schema._users Schema.supplyChainDb)
    contact <- all_ (Schema._contacts Schema.supplyChainDb)
    guard_ (Schema.contact_user1_id contact ==. val_ (Schema.UserId uid) &&.
            Schema.contact_user2_id contact ==. (Schema.UserId $ Schema.user_id user))
    pure user
  pure $ userTableToModel <$> userList


addContact :: (Member context '[HasDB],
               Member err     '[AsSqlError])
           => ST.User
           -> ST.UserId
           -> AppM context err Bool
addContact user userId = runDb $ addContactQuery user userId


addContactQuery :: ST.User -> ST.UserId -> DB context err Bool
addContactQuery (ST.User (ST.UserId uid1) _ _) (ST.UserId uid2) = do
  pKey <- U.newUUID
  r <- pg $ runInsertReturningList (Schema._contacts Schema.supplyChainDb) $
               insertValues [Schema.Contact Nothing pKey (Schema.UserId uid1) (Schema.UserId uid2)]
  pure $ verifyContact r (Schema.UserId uid1) (Schema.UserId uid2)



removeContact :: (Member context '[HasDB],
                  Member err     '[AsSqlError])
              => ST.User
              -> ST.UserId
              -> AppM context err Bool
removeContact user userId = runDb $ removeContactQuery user userId

-- | The current behaviour is, if the users were not contacts in the first
-- place, then the function returns false
-- otherwise, removes the user. Checks that the user has been removed,
-- and returns (not. userExists)
-- @todo Make ContactErrors = NotAContact | DoesntExist | ..
removeContactQuery :: ST.User -> ST.UserId -> DB context err Bool
removeContactQuery (ST.User firstId@(ST.UserId uid1) _ _) secondId@(ST.UserId uid2) = do
  contactExists <- isExistingContact firstId secondId
  if contactExists
    then do
      pg $ runDelete $ delete (Schema._contacts Schema.supplyChainDb)
              (\ contact ->
                Schema.contact_user1_id contact ==. val_ (Schema.UserId uid1) &&.
                Schema.contact_user2_id contact ==. val_ (Schema.UserId uid2))
      not <$> isExistingContact firstId secondId
  else pure False



-- Given a search term, search the users contacts for a user matching
-- that term
-- might want to use reg-ex features of postgres10 here:
-- PSEUDO:
-- SELECT user2, firstName, lastName FROM Contacts, Users WHERE user1 LIKE *term* AND user2=Users.id UNION SELECT user1, firstName, lastName FROM Contacts, Users WHERE user2 = ? AND user1=Users.id;" (uid, uid)
--

userSearch :: (Member context '[HasDB],
               Member err     '[AsSqlError])
           => ST.User
           -> Maybe GS1CompanyPrefix
           -> Maybe Text -- last name
           -> AppM context err [ST.User]
userSearch _ mgs1pfx mlastname = runDb $ userSearchQuery mgs1pfx mlastname


-- This is very ugly. I'm not familiar enough with Beam to know how to
-- structure it better though. It's probably best to actually have 2
-- (or 3 if we want to include first name)
-- query functions, check if the value is Just, if so, get the results, and then apply
-- an ordering/weighting function written in Haskell to the sum of the results.
-- I've left it in to illustrate
-- the idea behind the API, but we should definitely revisit it.
userSearchQuery :: Maybe GS1CompanyPrefix
                -> Maybe Text -- last name
                -> DB context err [ST.User]
userSearchQuery mpfx mlname =
  if all isNothing [() <$ mpfx, () <$ mlname] then pure []
  else fmap (map userTableToModel) $ pg $ runSelectReturningList $ select $ do
        user <- all_ (Schema._users Schema.supplyChainDb)
        for_ mpfx   $ \pfx   -> guard_ (user_biz_id user ==. (BizId $ val_ pfx))
        for_ mlname $ \lname -> guard_ (like_ (user_last_name user) (val_ lname))
        pure user



-- | Checks if a pair of userIds are recorded as a contact.
-- __Must be run in a transaction!__
isExistingContact :: ST.UserId -> ST.UserId -> DB context err Bool
isExistingContact (ST.UserId uid1) (ST.UserId uid2) = do
  r <- pg $ runSelectReturningList $ select $ do
        contact <- all_ (Schema._contacts Schema.supplyChainDb)
        guard_ (Schema.contact_user1_id contact  ==. (val_ . Schema.UserId $ uid1) &&.
                Schema.contact_user2_id contact  ==. (val_ . Schema.UserId $ uid2))
        pure contact
  pure $ verifyContact r (Schema.UserId uid1) (Schema.UserId uid2)


-- | Simple utility function to check that the users are part of the contact
-- typically used with the result of a query
verifyContact :: Eq (PrimaryKey Schema.UserT f) =>
                 [Schema.ContactT f] ->
                 PrimaryKey Schema.UserT f ->
                 PrimaryKey Schema.UserT f ->
                 Bool
verifyContact [insertedContact] uid1 uid2 =
                  (Schema.contact_user1_id insertedContact == uid1) &&
                  (Schema.contact_user2_id insertedContact == uid2)
verifyContact _ _ _ = False

