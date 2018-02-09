{-# LANGUAGE OverloadedStrings #-}
module MigrateScript (migrationStorage) where

import           Database.Beam.Postgres.Migrate
import           Database.Beam.Migrate.SQL.Tables
import           Database.Beam.Migrate.Types
import           Database.Beam.Postgres
import           StorageBeam

maxLen :: Word
maxLen = 120

-- length of the timezone offset
maxTzLen :: Word
maxTzLen = 10

-- pkSerialType :: DataType PgDataTypeSyntax UUID
pkSerialType = uuid

migrationStorage :: Migration PgCommandSyntax (CheckedDatabaseSettings Postgres SupplyChainDb)
migrationStorage =
  SupplyChainDb
    <$> createTable "users"
    (
      User
          (field "user_id" pkSerialType)
          (BizId (field "user_biz_id" text))
          (field "first_name" (varchar (Just maxLen)) notNull)
          (field "last_name" (varchar (Just maxLen)) notNull)
          (field "phone_number" (varchar (Just maxLen)) notNull)
          (field "password_hash" binaryLargeObject notNull)
          (field "email_address" (varchar (Just maxLen)) unique)
    )
    <*> createTable "keys"
    (
      Key
          (field "key_id" pkSerialType)
          (UserId (field "key_user_id" pkSerialType))
          (field "rsa_n" binaryLargeObject)
          (field "rsa_e" binaryLargeObject)
          (field "creation_time" timestamptz)
          (field "revocation_time" (maybeType timestamptz))
    )
    <*> createTable "businesses"
    (
      Business
          (field "biz_gs1_company_prefix" text) -- note is primary key
          (field "biz_name" (varchar (Just maxLen)) notNull)
          (field "biz_function" (varchar (Just maxLen)) notNull)
          (field "biz_site_name" (varchar (Just maxLen)) notNull)
          (field "biz_address" (varchar (Just maxLen)) notNull)
          (field "biz_lat" double)
          (field "biz_long" double)
    )
    <*> createTable "contacts"
    (
      Contact
          (field "contact_id" pkSerialType)
          (UserId (field "contact_user1_id" pkSerialType))
          (UserId (field "contact_user2_id" pkSerialType))
    )
    <*> createTable "labels"
    (
      Label
          (field "label_id" pkSerialType)
          (field "label_type" (varchar (Just maxLen)) notNull)
          (WhatId (field "label_what_id" pkSerialType))
          (field "label_gs1_company_prefix" (varchar (Just maxLen)) notNull)
          (field "item_reference" (maybeType $ varchar (Just maxLen)) notNull)
          (field "serial_number" (maybeType $ varchar (Just maxLen)) notNull)
          (field "state" (maybeType $ varchar (Just maxLen)))
          (field "lot" (maybeType $ varchar (Just maxLen)))
          (field "sgtin_filter_value" (maybeType $ varchar (Just maxLen)))
          (field "asset_type" (maybeType $ varchar (Just maxLen)))
          (field "quantity_amount" (maybeType double))
          (field "quantity_uom" (maybeType $ varchar (Just maxLen)))
    )
    <*> createTable "what_labels"
    (
      WhatLabel
          (field "what_label_id" pkSerialType)
          (WhatId (field "what_label_what_id" pkSerialType))
          (LabelId (field "what_label_label_id" pkSerialType))
    )
    <*> createTable "items"
    (
      Item
          (field "item_id" pkSerialType)
          (LabelId (field "item_label_id" pkSerialType))
          (field "item_description" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "transformations"
    (
      Transformation
          (field "transformation_id" pkSerialType)
          (field "transformation_description" (varchar (Just maxLen)) notNull)
          (BizId (field "transformation_biz_id" text))
    )
    <*> createTable "locations"
    (
      Location
          (field "location_id" text)
          (BizId (field "location_biz_id" text))
          -- this needs to be locationReferenceNum
          (field "location_lat" double)
          (field "location_long" double)
    )
    <*> createTable "events"
    (
      Event
          (field "event_id" pkSerialType)
          (field "foreign_event_id" (maybeType pkSerialType))
          -- (BizId (field "event_label_id" text))
          (UserId (field "event_created_by" pkSerialType))
          (field "json_event" (varchar (Just maxLen)) notNull)
    )
    <*> createTable "whats"
    (
      What
          (field "what_id" pkSerialType)
          (field "what_event_type" (maybeType text))
          (field "action" (maybeType text))
          (LabelId (field "parent" (maybeType pkSerialType)))
          (BizTransactionId (field "what_biz_transaction_id" (maybeType pkSerialType)))
          (TransformationId (field "what_transformation_id" (maybeType pkSerialType)))
          (EventId (field "what_event_id" pkSerialType))
    )
    <*> createTable "bizTransactions"
    (
      BizTransaction
          (field "biz_transaction_id" pkSerialType)
          (field "biz_transaction_type_id" (varchar (Just maxLen)))
          (field "biz_transaction_id_urn" (varchar (Just maxLen)))
          (EventId (field "biz_transaction_event_id" pkSerialType))
    )
    <*> createTable "whys"
    (
      Why
          (field "why_id" pkSerialType)
          (field "biz_step" (maybeType text))
          (field "disposition" (maybeType text))
          (EventId (field "why_event_id" pkSerialType))
    )
    <*> createTable "wheres"
    (
      Where
          (field "where_id" pkSerialType)
          (field "where_source_dest_type" (maybeType $ varchar (Just maxLen)) notNull)
          (field "where_gs1_location_id" (varchar (Just maxLen)) notNull)
          (field "where_location_field" (varchar (Just maxLen)) notNull)
          (EventId (field "where_event_id" pkSerialType))
    )
    <*> createTable "whens"
    (
      When
          (field "when_id" pkSerialType)
          (field "event_time" timestamptz notNull)
          (field "record_time" (maybeType timestamptz))
          (field "time_zone" (varchar (Just maxTzLen)) notNull)
          (EventId (field "when_event_id" pkSerialType))
    )
    <*> createTable "labelEvents"
    (
      LabelEvent
          (field "label_event_id" pkSerialType)
          (LabelId (field "label_event_label_id" pkSerialType))
          (EventId (field "label_event_event_id" pkSerialType))
    )
    <*> createTable "userEvent"
    (
      UserEvent
          (field "user_events_id" pkSerialType)
          (EventId (field "user_events_event_id" pkSerialType notNull))
          (UserId (field "user_events_user_id" pkSerialType notNull))
          (field "user_events_has_signed" boolean notNull)
          (UserId (field "user_events_added_by" pkSerialType notNull))
          (field "user_events_signedHash" (maybeType bytea))
    )
    <*> createTable "hashes"
    (
      Hashes
          (field "hashes_id" pkSerialType)
          (EventId (field "hashes_event_id" pkSerialType notNull))
          (field "hashes_hash" bytea notNull)
          (field "hashes_is_signed" boolean notNull)
          (UserId (field "hashes_signed_by_user_id" pkSerialType notNull))
          (KeyId (field "hashes_key_id" pkSerialType notNull))
    )
    <*> createTable "blockchain"
    (
      BlockChain
          (field "blockchain_id" pkSerialType)
          (EventId (field "blockchain_event_id" pkSerialType notNull))
          (field "blockchain_hash" bytea notNull)
          (field "blockchain_address" text notNull)
          (field "blockchain_foreign_id" int notNull)
    )
