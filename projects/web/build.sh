#!/bin/bash
set -e
PREV=$(pwd)
cd $(dirname "$BASH_SOURCE")

rm -fr dist/
rm -f deploy.tar

# run tests...

npm run-script build

tar -cvf deploy.tar index.html \
                    css/ \
                    node_modules/react/ \
                    node_modules/react-dom/ \
                    node_modules/auth0-js/ \
                    dist/

cd $PREV
