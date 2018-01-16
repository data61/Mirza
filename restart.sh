#!/usr/bin/env sh
# This script is for people who are lazy like me
# Run this to undo all database work, compile the code
# and rerun the server
# Ideally to be run during testing phase
echo Recreating the database
./manage_db.sh testsupplychainserver

GIVEN_OPT=$1
OPTION=${GIVEN_OPT:="some_random_text"}

echo Building the modules
if test $OPTION = '--from-scratch'
then
    stack clean
fi
stack build
stack exec supplyChainServer-exe -- run migrate jensoleg
