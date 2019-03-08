#!/bin/bash

set -ex

# Install the awscli tool and add it to PATH
pip install --user awscli
export PATH=$PATH:$HOME/.local/bin

# Authenticate with AWS and push the docker images so that they can be deployed to the server
eval $(aws ecr get-login --no-include-email --region ap-southeast-2)
docker push "$1"
