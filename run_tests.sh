#!/usr/bin/env bash


# How to use: ./run_tests.sh --coverage --ta "-p \"addPublicKey test 1\""


### This loop splits arguments into those before "--ta" and those after
#   ./run_tests.sh --coverage --ta "-p \"addPublicKey test 1\""
# becomes
#   stack test --fast --coverage --ta "-j1 -p \"addPublicKey test 1\""

stack build --fast

psql -c 'DROP DATABASE testsupplychainserver;' && \
psql -c 'DROP DATABASE testbusinessregistry;' && \
psql -c 'CREATE DATABASE testsupplychainserver;' && \
psql -c 'CREATE DATABASE testbusinessregistry;' && \
stack exec supplyChainServer -- --init-db -c 'dbname=testsupplychainserver' && \
echo 'YES' | stack exec businessRegistry -- initdb -c 'dbname=testbusinessregistry' && \
echo "Databases successfully recreated."

n=0
unset args  # Force args to be an empty array (it could be an env var on entry)
for i in "$@"
do
  if [[ "$i" != "--ta" ]]; then
    args[$((n++))]="$i"
    shift
  else
    shift
    break
  fi
done

stack test --fast "${args[@]}" --ta "-j1 $*"
