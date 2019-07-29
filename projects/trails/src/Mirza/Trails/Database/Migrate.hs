{-# LANGUAGE OverloadedStrings #-}


module Mirza.Trails.Database.Migrate ( migrations
                                          , runMigrationSimple
                                          , dropTablesSimple
                                          ) where


import           Mirza.Trails.Database.Schema.V0001

import           Mirza.Common.Database

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types

import           Data.Text.Encoding


migrations :: [Migration]
migrations = [ m_0001 ]

m_0001 :: Migration
m_0001 conn = do
  _ <- execute_ conn $ Query $ encodeUtf8 $ "CREATE TABLE " <> entriesTName
                                                              <> "("
                                                                <> entriesTFieldSignature        <> " TEXT PRIMARY KEY, "
                                                                <> entriesTFieldTimestamp        <> " TIMESTAMP NOT NULL, "
                                                                <> entriesTFieldGS1CompanyPrefix <> " TEXT NOT NULL, "
                                                                <> entriesTFieldEventId          <> " UUID NOT NULL, "
                                                                <> "last_update TIMESTAMP "
                                                              <> ");"
  createTrigger conn $ Query $ encodeUtf8 entriesTName

  _ <- execute_ conn $ Query $ encodeUtf8 $ "CREATE TABLE " <> parentsTName
                                                              <> "("
                                                                <> parentsTFieldSignature       <> " TEXT NOT NULL REFERENCES " <> entriesTName <> "(" <> entriesTFieldSignature <> ") ON DELETE CASCADE, "
                                                                <> parentsTFieldParentSignature <> " TEXT NOT NULL REFERENCES " <> entriesTName <> "(" <> entriesTFieldSignature <> "), "
                                                                <> "last_update TIMESTAMP, "
                                                                <> "PRIMARY KEY(" <> parentsTFieldSignature <> ", " <> parentsTFieldParentSignature <>")"
                                                              <> ");"
  createTrigger conn $ Query $ encodeUtf8 parentsTName
