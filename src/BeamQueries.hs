{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BeamQueries where

import qualified Model as M
import Crypto.Scrypt
import Data.Text.Encoding
import Database.PostgreSQL.Simple
import Database.Beam as B
import Database.Beam.Backend.SQL.BeamExtensions
import StorageBeam
import AppConfig (AppM, runDb)
import Data.Maybe (fromJust)

insertUser :: EncryptedPass -> M.NewUser -> AppM M.UserID
insertUser pass (M.NewUser phone email firstName lastName biz password) = do

  [insertedUser] <- runDb $ runInsertReturningList (_users supplyChainDb) $
                    insertValues [(User (Auto Nothing) --0
                    (BizId . Auto . Just $ biz) firstName lastName phone password email)]
                    -- (BizId . Auto. Just . fromIntegral $ biz) firstName lastName phone password email)]
  -- print insertedUser
  return (user_id insertedUser)

-- |
newUser :: M.NewUser -> AppM M.UserID
newUser userInfo@(M.NewUser _ _ _ _ _ password) = do
    hash <- liftIO $ encryptPassIO' (Pass $ encodeUtf8 password)
    insertUser hash userInfo
