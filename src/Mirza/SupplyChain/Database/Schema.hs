module Mirza.SupplyChain.Database.Schema
  ( module Current
  , migration
  , supplyChainDb
  , checkedSupplyChainDb
  , primaryKey
  ) where

-- import           Control.Arrow ((>>>))

import           Database.Beam                           (DatabaseSettings)
import           Database.Beam                           as B
import           Database.Beam.Migrate.Types             hiding (migrateScript)
import           Database.Beam.Postgres                  (PgCommandSyntax,
                                                          Postgres)
import           Database.Beam.Schema.Tables             (primaryKey)

import           Mirza.SupplyChain.Database.Schema.V0001 as Current hiding
                                                                     (migration)

import qualified Mirza.SupplyChain.Database.Schema.V0001 as V0001


migration :: MigrationSteps PgCommandSyntax () (CheckedDatabaseSettings Postgres Current.SupplyChainDb)
migration = migrationStep "Initial commit" V0001.migration
           -- >>> migrationStep """todo comment""" V0002.migration

-- supplyChainDb :: DatabaseSettings Postgres Current.SupplyChainDb
-- supplyChainDb = unCheckDatabase checkedSupplyChainDb

checkedSupplyChainDb :: CheckedDatabaseSettings Postgres Current.SupplyChainDb
checkedSupplyChainDb = evaluateDatabase migration

-- | Everything that comes after ``withDbModification`` is primarily
-- foreign keys that have been forced to retain their own names
supplyChainDb :: DatabaseSettings Postgres SupplyChainDb
supplyChainDb = defaultDbSettings
  `withDbModification`
  dbModification
    {
      _users =
        modifyTable (const "users") $
        tableModification {
          user_biz_id = BizId (fieldNamed "user_biz_id")
        }
    , _contacts =
        modifyTable (const "contacts") $
        tableModification {
         contact_user1_id = UserId (fieldNamed "contact_user1_id")
        , contact_user2_id = UserId (fieldNamed "contact_user2_id")
        }
    , _labels =
        modifyTable (const "labels") $
        tableModification {
          label_what_id = WhatId (fieldNamed "label_what_id")
        }
    , _what_labels =
        modifyTable (const "what_labels") $
        tableModification {
          what_label_what_id = WhatId (fieldNamed "what_label_what_id")
        , what_label_label_id = LabelId (fieldNamed "what_label_label_id")
        }
    , _items =
        modifyTable (const "items") $
        tableModification {
          item_label_id = LabelId (fieldNamed "item_label_id")
        }
    , _transformations =
        modifyTable (const "transformations") $
        tableModification {
          transformation_biz_id = BizId (fieldNamed "transformation_biz_id")
        }
    , _locations =
        modifyTable (const "locations") $
        tableModification {
          location_biz_id = BizId (fieldNamed "location_biz_id")
        }
    , _events =
        modifyTable (const "events") $
        tableModification {
          event_created_by = UserId (fieldNamed "event_created_by")
        }
    , _whats =
        modifyTable (const "whats") $
        tableModification {
          what_parent = LabelId (fieldNamed "what_parent")
        , what_biz_transaction_id = BizTransactionId (fieldNamed "what_biz_transaction_id")
        , what_transformation_id = TransformationId (fieldNamed "what_transformation_id")
        , what_event_id = EventId (fieldNamed "what_event_id")
        }
    , _biz_transactions =
        modifyTable (const "biz_transactions") $
        tableModification {
          biz_transaction_event_id = EventId (fieldNamed "biz_transaction_event_id")
        }
    , _whys =
        modifyTable (const "whys") $
        tableModification {
          why_event_id = EventId (fieldNamed "why_event_id")
        }
    , _wheres =
        modifyTable (const "wheres") $
        tableModification {
          where_event_id = EventId (fieldNamed "where_event_id")
        }
    , _whens =
        modifyTable (const "whens") $
        tableModification {
          when_event_id = EventId (fieldNamed "when_event_id")
        }
    , _label_events =
        modifyTable (const "label_events") $
        tableModification {
          label_event_label_id = LabelId (fieldNamed "label_event_label_id")
        , label_event_event_id = EventId (fieldNamed "label_event_event_id")
        }
    , _user_events =
        modifyTable (const "user_event") $
        tableModification {
          user_events_event_id = EventId (fieldNamed "user_events_event_id")
        , user_events_user_id = UserId (fieldNamed "user_events_user_id")
        , user_events_owner = UserId (fieldNamed "user_events_added_by")
        }
    , _signatures =
        modifyTable (const "signature") $
        tableModification {
          signature_event_id = EventId (fieldNamed "signature_event_id")
        }
    , _hashes =
        modifyTable (const "hashes") $
        tableModification {
          hashes_event_id = EventId (fieldNamed "hashes_event_id")
        , hashes_signed_by_user_id = UserId (fieldNamed "hashes_signed_by_user_id")
        }
    , _blockchain =
        modifyTable (const "blockchain") $
        tableModification {
          blockchain_event_id = EventId (fieldNamed "blockchain_event_id")
        }
    }

