#!/bin/bash
cd "$(dirname "$0")/../projects/br_scs"
cat stack.yaml Mirza.cabal | sha1sum | sed 's/[ -]*$//'
