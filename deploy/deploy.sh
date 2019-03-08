#!/bin/bash

set -e

# Update the docker-compose.yml to refer to the images that have been built (rather than latest)
sed -i "s#latest#$TRAVIS_COMMIT#g" deploy/docker-compose.yml

# Set correct permissions on the deployment ssh key
chmod 0600 ~/.ssh/deployment.pem

# Send the docker-compose.yml, create_databases.sh and get_and_start_new_version.sh scripts to the AWS server.
scp -i ~/.ssh/deployment.pem -o StrictHostKeyChecking=no deploy/docker-compose.yml ec2-user@$SERVER_IP:/home/ec2-user/
scp -i ~/.ssh/deployment.pem -o StrictHostKeyChecking=no deploy/create_databases.sh ec2-user@$SERVER_IP:/home/ec2-user/
scp -i ~/.ssh/deployment.pem -o StrictHostKeyChecking=no deploy/get_and_start_new_version.sh ec2-user@$SERVER_IP:/home/ec2-user/

# Execute the get_and_start_new_version script to pull in and start using the latest version of code.
ssh -i ~/.ssh/deployment.pem -o StrictHostKeyChecking=no ec2-user@$SERVER_IP ./get_and_start_new_version.sh
