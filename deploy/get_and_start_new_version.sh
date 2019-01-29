#!/bin/bash

# This script is sent to the aws server, and executed with every deployment.
# This script also assumes the existence of a .env file alongside the docker-compose.yml file.

# Authenticate with ECR so that 'docker pull' can be used.
$(aws ecr get-login --no-include-email --region ap-southeast-2)

# Stop and remove the currently running version of code
docker-compose stop supplyChainServer businessRegistry private-ethereum-blockchain blockchain-api-server db || echo "no old containers running"
docker-compose rm -f supplyChainServer businessRegistry private-ethereum-blockchain blockchain-api-server db || echo "no containers to remove"

# Get the new images (defined in the docker-compose.yml) 
docker-compose pull

# Get the secrets from AWS Secrets Manager and put them in a .env file.

POSTGRES_USER=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .POSTGRES_USER)
POSTGRES_PASSWORD=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .POSTGRES_PASSWORD)
ETH_NODE_PROTOCOL=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .ETH_NODE_PROTOCOL)
ETH_NODE_HOST=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .ETH_NODE_HOST)
ETH_NODE_PORT=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .ETH_NODE_PORT)
ETH_ADMIN_ACCOUNT_PRIVATE_KEY=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .ETH_ADMIN_ACCOUNT_PRIVATE_KEY)
BR_USER=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .BR_USER)
BR_PASSWORD=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .BR_PASSWORD)

filepath='/home/ec2-user/.env'

echo "POSTGRES_USER=$POSTGRES_USER
POSTGRES_PASSWORD=$POSTGRES_PASSWORD
ETH_NODE_PROTOCOL=$ETH_NODE_PROTOCOL
ETH_NODE_HOST=$ETH_NODE_HOST
ETH_NODE_PORT=$ETH_NODE_PORT
ETH_ADMIN_ACCOUNT_PRIVATE_KEY=$ETH_ADMIN_ACCOUNT_PRIVATE_KEY
BR_USER=$BR_USER
BR_PASSWORD=$BR_PASSWORD" > ${filepath}

# Start the database (either an empty postgres container, or an existing database)
docker-compose up -d db

# Start the services
docker-compose up -d supplyChainServer businessRegistry private-ethereum-blockchain blockchain-api-server

# Run the initdb scripts for scs and br. (UNCOMMENT WHEN NEEDED FOR A CLEAN DEPLOYMENT)
# docker-compose up -d dbpopulate-br
# docker-compose up -d dbpopulate-scs

# remove all unused docker images and exited containers
docker system prune -a --force
