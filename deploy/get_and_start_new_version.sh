#!/bin/bash

# This script is sent to the aws server, and executed with every deployment.
# This script also assumes the existence of a .env file alongside the docker-compose.yml file.

# Authenticate with ECR so that 'docker pull' can be used.
$(aws ecr get-login --no-include-email --region ap-southeast-2)

# Stop and remove the currently running version of code
docker-compose stop web orgRegistry sci041edapi sci041scs sci033edapi sci033scs || echo "no old containers running"
docker-compose rm -f web orgRegistry sci041edapi sci041scs sci033edapi sci033scs || echo "no containers to remove"

# Get the new images (defined in the docker-compose.yml)
docker-compose pull

# Get the secrets from AWS Secrets Manager and put them in a .env file.
POSTGRES_USER=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .POSTGRES_USER)
POSTGRES_PASSWORD=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .POSTGRES_PASSWORD)
ETH_NODE_PROTOCOL=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .ETH_NODE_PROTOCOL)
ETH_NODE_HOST=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .ETH_NODE_HOST)
ETH_NODE_PORT=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .ETH_NODE_PORT)
ETH_ADMIN_ACCOUNT_PRIVATE_KEY=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .ETH_ADMIN_ACCOUNT_PRIVATE_KEY)
OR_USER=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .OR_USER)
OR_PASSWORD=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .OR_PASSWORD)
OAUTH_SUB=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .OAUTH_SUB)
SCI033_OAUTH_SUB=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .SCI033_OAUTH_SUB)
SCI041_OAUTH_SUB=$(aws --region ap-southeast-2 secretsmanager get-secret-value --secret-id development | jq -r .SecretString | sed 's/\\//g' | jq -r .SCI041_OAUTH_SUB)

echo "POSTGRES_USER=$POSTGRES_USER
POSTGRES_PASSWORD=$POSTGRES_PASSWORD
OR_USER=$OR_USER
OR_PASSWORD=$OR_PASSWORD
OAUTH_SUB=$OAUTH_SUB" > '/home/ec2-user/.env'

echo "JWK_CLIENT_IDS=$SCI033_OAUTH_SUB
EDAPI_DB_CONN=postgresql://${POSTGRES_USER}:${POSTGRES_PASSWORD}@db/sci033edapi
DEST_HOST=sci033scs
DEST_PORT=8000" > '/home/ec2-user/sci033.env'

echo "JWK_CLIENT_IDS=$SCI041_OAUTH_SUB
EDAPI_DB_CONN=postgresql://${POSTGRES_USER}:${POSTGRES_PASSWORD}@db/sci041edapi
DEST_HOST=sci041scs
DEST_PORT=8000" > '/home/ec2-user/sci041.env'

# Start the database (either an empty postgres container, or an existing database)
docker-compose up -d db

echo Waiting 10 seconds for the db to finish starting...
sleep 10

# Start the services
docker-compose up -d web orgRegistry sci041edapi sci041scs sci033edapi sci033scs # private-ethereum-blockchain blockchain-api-server

# remove all unused docker images and exited containers
# docker system prune -a --force
