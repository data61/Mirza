#!/bin/bash

# This script is sent to the aws server, and executed with every deployment
set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$POSTGRES_DB" <<-EOSQL
  CREATE DATABASE devsupplychainserver;
  CREATE DATABASE devbusinessregistry;
EOSQL