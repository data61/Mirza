{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BeamQueries where

import qualified Model as M
-- import qualified CryptHash as C
-- import qualified Data.Text as Txt
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
import Database.PostgreSQL.Simple
-- import Control.Lens
import Database.Beam as B
-- import Database.Beam.Postgres
-- import Database.Beam.Backend
import Database.Beam.Backend.SQL.BeamExtensions
-- import Database.PostgreSQL.Simple.FromField
-- import Database.Beam.Backend.SQL
import StorageBeam
import AppConfig (dbFunc, AppM, runDb)

insertUser :: EncryptedPass -> M.NewUser -> AppM M.UserID
insertUser pass (M.NewUser phone email firstName lastName biz password) = do

  [insertedUser] <- runDb $ runInsertReturningList (_users supplyChainDb) $
                    insertValues [(User 0 --(Auto Nothing)
                    (BizId biz) firstName lastName phone password email)]
                    -- (BizId . Auto. Just . fromIntegral $ biz) firstName lastName phone password email)]
  -- print insertedUser
  return (user_id insertedUser)

-- |
newUser :: M.NewUser -> AppM M.UserID
newUser userInfo@(M.NewUser _ _ _ _ _ password) = do
    hash <- liftIO $ encryptPassIO' (Pass $ encodeUtf8 password)
    insertUser hash userInfo
