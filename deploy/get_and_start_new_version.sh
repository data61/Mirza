#!/bin/bash

# This script is sent to the aws server, and executed with every deployment.
# This script also assumes the existence of a .env file alongside the docker-compose.yml file.

# Authenticate with ECR so that 'docker pull' can be used.
$(aws ecr get-login --no-include-email --region ap-southeast-2)

# Stop and remove the currently running version of code
docker-compose stop supplyChainServer businessRegistry private-ethereum-blockchain blockchain-api-server || echo "no old containers running"
docker-compose rm -f supplyChainServer businessRegistry private-ethereum-blockchain blockchain-api-server || echo "no containers to remove"

# Get the new images (defined in the docker-compose.yml) 
docker-compose pull

# Start the database (either an empty postgres container, or an existing database)
docker-compose up -d db

# Run the initdb scripts for scs and br. (UNCOMMENT WHEN NEEDED FOR DEPLOYMENT)
# docker-compose up -d dbpopulate-scs
# docker-compose up -d dbpopulate-br

# Start the services
docker-compose up -d supplyChainServer businessRegistry private-ethereum-blockchain blockchain-api-server

# remove all unused docker images and exited containers
docker system prune -a --force
