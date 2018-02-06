#!/usr/bin/env sh
# This script is for people who are lazy like me
# Run this to undo all database work, compile the code
# and rerun the server
# Ideally to be run during testing phase

export DBNAME="devsupplychainserver"

echo Recreating the database
./manage_db.sh testsupplychainserver

# Defaulting opt to avoid error
GIVEN_OPT=$1
OPTION=${GIVEN_OPT:="some_random_text"}

echo Building the modules
if test $OPTION = '--clean'
then
    stack clean
fi

stack build
stack exec supplyChainServer-exe -- -i

echo "Now inserting some dummy companies"
echo
psql \
    -X \
    --echo-all \
    --set AUTOCOMMIT=on \
    --set ON_ERROR_STOP=on \
    $DBNAME \
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
