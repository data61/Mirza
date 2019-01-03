#!/bin/bash

# This script is sent to the aws server, and executed with every deployment.
# This script also assumes the existence of a .env file alongside the docker-compose.yml file.

docker-compose down
docker-compose pull
docker-compose up -d db
docker-compose up -d dbpopulate-scs
docker-compose up -d dbpopulate-br
docker-compose up -d supplyChainServer businessRegistry