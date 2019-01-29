#!/bin/bash

set -ex

# Install the awscli tool and add it to PATH

pip install --user awscli
export PATH=$PATH:$HOME/.local/bin

# Build deployment docker images
docker build -f Mirza.Dockerfile --target PKG-SCS -t ${DOCKER_REPO}/supplychainserver:$TRAVIS_COMMIT .
docker build -f Mirza.Dockerfile --target PKG-BR -t ${DOCKER_REPO}/businessregistry:$TRAVIS_COMMIT .

# Update the docker-compose.yml to refer to the images that have been built (rather than latest)
sed -i "s#latest#$TRAVIS_COMMIT#g" deploy/docker-compose.yml

# Make logs silent again so we don't get spammed by travis-cis inner workings.
set +x

# Authenticate with AWS and push the docker images so that they can be deployed to the server
eval $(aws ecr get-login --no-include-email --region ap-southeast-2)
docker push ${DOCKER_REPO}/supplychainserver:$TRAVIS_COMMIT
docker push ${DOCKER_REPO}/businessregistry:$TRAVIS_COMMIT

# Set correct permissions on the deployment ssh key
chmod 0600 ~/.ssh/deployment.pem

# Send the docker-compose.yml, create_databases.sh and get_and_start_new_version.sh scripts to the AWS server.
scp -i ~/.ssh/deployment.pem -o StrictHostKeyChecking=no deploy/docker-compose.yml ec2-user@$SERVER_IP:/home/ec2-user/
scp -i ~/.ssh/deployment.pem -o StrictHostKeyChecking=no deploy/create_databases.sh ec2-user@$SERVER_IP:/home/ec2-user/
scp -i ~/.ssh/deployment.pem -o StrictHostKeyChecking=no deploy/get_and_start_new_version.sh ec2-user@$SERVER_IP:/home/ec2-user/

# Execute the get_and_start_new_version script to pull in and start using the latest version of code.
ssh -i ~/.ssh/deployment.pem -o StrictHostKeyChecking=no ec2-user@$SERVER_IP ./get_and_start_new_version.sh