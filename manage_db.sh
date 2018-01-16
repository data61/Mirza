#!/usr/bin/env bash

set -e
set -u

if [ $# -lt 1 ]; then
cat << eof
please enter a name of the test database (testsupplychainserver)
eof
    exit 1
fi


DB_NAME=$1

psql \
    --echo-all \
    --set AUTOCOMMIT=off \
    --set ON_ERROR_STOP=on << EOF
DROP DATABASE IF EXISTS testsupplychainserver;
CREATE DATABASE testsupplychainserver;
EOF

psql_exit_status=$?
if [ $psql_exit_status != 0 ]; then
    echo "psql failed while trying to run this sql script" 1>&2
    exit $psql_exit_status
fi

echo "sql script successful"
exit 0
