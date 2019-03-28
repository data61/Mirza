#!/bin/bash
cd "$(dirname "$0")/.."
cat stack.yaml Mirza.cabal | sha1sum | sed 's/[ -]*$//'
