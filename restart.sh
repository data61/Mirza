#!/usr/bin/env sh
# This script is for people who are lazy like me
# Run this to undo all database work, compile the code
# and rerun the server
# Ideally to be run during testing phase

export SCS_DBNAME="devsupplychainserver"
export BR_DBNAME="devmirzabusinessregistry"

echo Recreating the database
./manage_db.sh $SCS_DBNAME
./manage_db.sh $BR_DBNAME

# Defaulting opt to avoid error
GIVEN_OPT=$1
OPTION=${GIVEN_OPT:="some_random_text"}

echo Building the modules
if test $OPTION = '--clean'
then
    stack clean
fi

stack build --fast
stack exec supplyChainServer-exe -- --init-db --brhost localhost --brport 8200
stack exec businessRegistry -- initdb

# eventuially, we will get an updated list from ASIC and populate the db
echo "Now inserting some dummy companies"
echo
psql \
    -X \
    --echo-all \
    --set AUTOCOMMIT=on \
    --set ON_ERROR_STOP=on \
    $BR_DBNAME \
    << EOF
INSERT INTO businesses \
    (biz_gs1_company_prefix, biz_name, biz_function, biz_site_name, biz_address, biz_lat, biz_long) \
    VALUES \
    ('4012345', 'Lomondo', 'Truck-driver', 'Holsworthy', '123 Holsworthy St', 123.456, 89.034);

INSERT INTO businesses \
    (biz_gs1_company_prefix, biz_name, biz_function, biz_site_name, biz_address) \
    VALUES \
    ('4000001', 'Manny''s Olive Oil', 'Farmenter', 'Warwick Farm', 'Farmland Av');

INSERT INTO businesses \
    (biz_gs1_company_prefix, biz_name, biz_function, biz_site_name, biz_address) \
    VALUES \
    ('0614141', 'Pulitzer Harvest', 'Harvester', 'Keskuskatu', 'ul. Filtrowa 68');
EOF

echo "Done"

export START_IN=2
echo "Starting the server in $START_IN s. Feed me a SIGINT (CTRL+C or equivalent) to stop."

sleep $START_IN
google-chrome "http://localhost:8000/swagger-ui/"

# The sleep here is to queue this process so that it fires AFTER
# the supplyChainServer-exe executable is run
(sleep 2 && \
echo "Inserting a user. Username: abc@gmail.com, Password: password"
curl -X POST "http://localhost:8000/newUser" \
    -H "accept: application/json;charset=utf-8"\
    -H "Content-Type: application/json;charset=utf-8"\
    -d "{ \"newUserPhoneNumber\": \"0412\", \"newUserEmailAddress\": \"abc@gmail.com\", \"newUserFirstName\": \"sajid\", \"newUserLastName\": \"anower\", \"newUserCompany\": \"4000001\", \"newUserPassword\": \"password\"}")&
echo; echo

stack exec businessRegistry -- server &

stack exec supplyChainServer-exe -- --brhost localhost --brport 8200 -e Dev
