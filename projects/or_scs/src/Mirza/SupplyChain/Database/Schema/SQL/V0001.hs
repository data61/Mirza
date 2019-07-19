{-# LANGUAGE QuasiQuotes #-}

module Mirza.SupplyChain.Database.Schema.SQL.V0001 where

import Mirza.Common.Database

import Data.String ( fromString )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ (sql)


-- SQL

m_0001 :: Migration
m_0001 conn = do
  [(Only x)] <- query_ conn "SELECT current_database();"
  _ <- execute_ conn $ "ALTER DATABASE " <> fromString x <>" SET client_min_messages TO WARNING";
  
  
  _ <- execute_ conn [sql|
CREATE OR REPLACE FUNCTION public.sync_lastmod() RETURNS trigger
  LANGUAGE plpgsql
  AS $$ BEGIN NEW.last_update := NOW() AT TIME ZONE 'UTC'; RETURN NEW; END; $$;
|]

  _ <- execute_ conn [sql|
CREATE TABLE public.orgs (
  last_update timestamp without time zone DEFAULT now(),
  org_gs1_company_prefix text PRIMARY KEY,
  org_name character varying(120) NOT NULL
);
|]
  
  _ <- execute_ conn [sql|
CREATE TABLE public.locations (
  last_update timestamp without time zone DEFAULT now(),
  location_id text PRIMARY KEY,
  location_org_id text REFERENCES orgs(org_gs1_company_prefix) ON DELETE CASCADE,
  location_function character varying(120) NOT NULL,
  location_site_name character varying(120) NOT NULL,
  location_address character varying(120) NOT NULL,
  location_lat double precision,
  location_long double precision
);
|]
      
  _ <- execute_ conn [sql|
CREATE TABLE public.events (
  last_update timestamp without time zone DEFAULT now(),
  event_id uuid PRIMARY KEY,
  event_foreign_event_id uuid,
  event_json json NOT NULL,
  event_to_sign bytea NOT NULL UNIQUE
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.labels (
    last_update timestamp without time zone DEFAULT now(),
    label_id uuid PRIMARY KEY,
    label_gs1_company_prefix text NOT NULL,
    label_item_reference text,
    label_serial_number text,
    label_state character varying(120),
    label_lot text,
    label_sgtin_filter_value text,
    label_asset_type text,
    label_quantity_amount double precision,
    label_quantity_uom text,
    label_urn text NOT NULL UNIQUE
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.label_events (
    last_update timestamp without time zone DEFAULT now(),
    label_event_id uuid PRIMARY KEY,
    label_event_label_id uuid REFERENCES labels(label_id) ON DELETE CASCADE,
    label_event_event_id uuid REFERENCES events(event_id) ON DELETE CASCADE,
    label_event_label_type text
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.whats (
    last_update timestamp without time zone DEFAULT now(),
    what_id uuid PRIMARY KEY,
    what_event_type text,
    what_action text,
    what_parent uuid REFERENCES labels(label_id) ON DELETE CASCADE,
    what_org_transaction_id uuid,
    what_transformation_id uuid,
    what_event_id uuid REFERENCES events(event_id) ON DELETE CASCADE
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.what_labels (
    last_update timestamp without time zone DEFAULT now(),
    what_label_id uuid PRIMARY KEY,
    what_label_what_id uuid REFERENCES whats(what_id) ON DELETE CASCADE,
    what_label_label_id uuid REFERENCES labels(label_id) ON DELETE CASCADE,
    what_label_label_type text
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.org_transactions (
    last_update timestamp without time zone DEFAULT now(),
    org_transaction_id uuid PRIMARY KEY,
    org_transaction_type_id character varying(120),
    org_transaction_id_urn character varying(120),
    org_transaction_event_id uuid REFERENCES events(event_id) ON DELETE CASCADE
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.transformations (
    last_update timestamp without time zone DEFAULT now(),
    transformation_id uuid PRIMARY KEY,
    transformation_description character varying(120) NOT NULL,
    transformation_org_id text REFERENCES orgs(org_gs1_company_prefix) ON DELETE CASCADE
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.whens (
    last_update timestamp without time zone DEFAULT now(),
    when_id uuid PRIMARY KEY,
    when_event_time timestamp without time zone NOT NULL,
    when_record_time timestamp without time zone,
    when_time_zone character varying(10) NOT NULL,
    when_event_id uuid REFERENCES events(event_id) ON DELETE CASCADE
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.wheres (
    last_update timestamp without time zone DEFAULT now(),
    where_id uuid PRIMARY KEY,
    where_gs1_company_prefix text,
    where_source_dest_type text,
    where_gs1_location_id text,
    where_location_field text NOT NULL,
    where_sgln_ext text,
    where_geo text,
    where_event_id uuid REFERENCES events(event_id) ON DELETE CASCADE
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.whys (
    last_update timestamp without time zone DEFAULT now(),
    why_id uuid PRIMARY KEY,
    why_org_step text,
    why_disposition text,
    why_event_id uuid REFERENCES events(event_id) ON DELETE CASCADE
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.blockchain (
    last_update timestamp without time zone DEFAULT now(),
    blockchain_id uuid PRIMARY KEY,
    blockchain_event_id uuid NOT NULL REFERENCES events(event_id) ON DELETE CASCADE,
    blockchain_hash bytea NOT NULL,
    blockchain_address text NOT NULL,
    blockchain_foreign_id integer NOT NULL
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.hashes (
    last_update timestamp without time zone DEFAULT now(),
    hashes_id uuid PRIMARY KEY,
    hashes_event_id uuid NOT NULL REFERENCES events(event_id) ON DELETE CASCADE,
    hashes_hash bytea NOT NULL,
    hashes_is_signed boolean NOT NULL,
    hashes_key_id uuid NOT NULL
);
|]
  _ <- execute_ conn [sql|
CREATE TABLE public.signatures (
    last_update timestamp without time zone DEFAULT now(),
    signature_id uuid PRIMARY KEY,
    signature_event_id uuid NOT NULL REFERENCES events(event_id) ON DELETE CASCADE,
    signature_key_id uuid NOT NULL,
    signature_signature json NOT NULL,
    signature_timestamp timestamp without time zone NOT NULL
);
|]

  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.orgs FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.labels FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.transformations FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.events FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.org_transactions FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.whats FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.what_labels FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.locations FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.whys FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.wheres FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.whens FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.label_events FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.signatures FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.hashes FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]
  _ <- execute_ conn [sql|
CREATE TRIGGER sync_lastmod BEFORE INSERT OR UPDATE ON public.blockchain FOR EACH ROW EXECUTE PROCEDURE public.sync_lastmod();
|]

  pure ()
