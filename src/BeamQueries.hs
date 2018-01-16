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
import Data.Text.Encoding
import Data.ByteString.Char8 (pack, unpack)
-- import qualified Data.ByteString as ByteString
-- import qualified Data.Text.Lazy as TxtL

-- import qualified Data.List.NonEmpty as NonEmpty
import Database.PostgreSQL.Simple
-- import Control.Lens
import Database.Beam as B
-- import Database.Beam.Postgres
-- import Database.Beam.Backend
import Database.Beam.Backend.SQL.BeamExtensions
-- import Database.PostgreSQL.Simple.FromField
-- import Database.Beam.Backend.SQL
import StorageBeam

insertUser :: Connection -> EncryptedPass -> M.NewUser -> IO M.UserID
insertUser conn pass (M.NewUser phone email firstName lastName biz password) = do

  [insertedUser] <- dbFunc conn $ runInsertReturningList (_users supplyChainDb) $
                    insertValues [(User 0 --(Auto Nothing)
                    (BizId biz) firstName lastName phone password email)]
                    -- (BizId . Auto. Just . fromIntegral $ biz) firstName lastName phone password email)]
  -- print insertedUser
  return (user_id insertedUser)

-- |
newUser :: Connection -> M.NewUser -> IO M.UserID
newUser conn userInfo@(M.NewUser phone email firstName lastName biz password) = do
    hash <- encryptPassIO' (Pass $ encodeUtf8 password)
    insertUser conn hash userInfo

-- Basic Auth check using Scrypt hashes.
-- authCheck :: Connection -> DBFunc -> M.EmailAddress -> M.Password -> IO (Maybe M.User)
-- authCheck conn dbFunc email password = do
--   dbFunc conn $ do
--     r <- runSelectReturningList $ select theUser
--   where
--     theUser = do
--       r <- query_ conn "SELECT rowID, firstName, lastName, passwordHash FROM Users WHERE emailAddress = ?;" $ Only $ unpack email
--       if length r == 0
--         then return Nothing
--         else do
--           let (uid, firstName, lastName, hash) = head r
--           if verifyPass' (Pass password) (EncryptedPass hash)
--               then return $ Just $ M.User uid firstName lastName
--               else return Nothing
