#!/bin/bash
cd "$(dirname "$0")/../projects/or_scs"
cat stack.yaml Mirza.cabal | sha1sum | sed 's/[ -]*$//'
