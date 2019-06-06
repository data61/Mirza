#!/usr/bin/env sh
# This script is for people who are lazy like me
# Run this to undo all database work, compile the code
# and rerun the server
# Ideally to be run during testing phase

export SCS_DBNAME="devsupplychainserver"
export OR_DBNAME="devmirzaorgregistry"

echo Recreating the database
./manage_db.sh $SCS_DBNAME
./manage_db.sh $OR_DBNAME

# Defaulting opt to avoid error
GIVEN_OPT=$1
OPTION=${GIVEN_OPT:="some_random_text"}

echo Building the modules
if test $OPTION = '--clean'
then
    stack clean
fi

stack build --fast
stack exec supplyChainServer -- --init-db --orhost localhost --orport 8200
echo 'YES' | stack exec orgRegistry -- initdb

export START_IN=2
echo "Starting the server in $START_IN s. Feed me a SIGINT (CTRL+C or equivalent) to stop."

sleep $START_IN
google-chrome "http://localhost:8000/swagger-ui/"

stack exec orgRegistry -- server &

stack exec supplyChainServer -- --orhost localhost --orport 8200 -e Dev
