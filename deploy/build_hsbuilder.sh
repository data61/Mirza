#!/bin/bash
set -e

if [ -z "$1" ]; then
	echo "Usage: build_hsbuilder.sh <IMAGE>"
	exit 1
fi

cd "$(dirname "$0")/.."

IMAGE=$1
HASH=$(./deploy/dephash.sh)
DATE=$(date '+%Y%m%d-%H%M')

if docker pull "${IMAGE}:${HASH}"; then
	echo "Skipping, ${HASH} already exists..."
	exit 1
fi

docker build -f HsBuilder.Dockerfile -t "${IMAGE}:latest" -t "${IMAGE}:${HASH}" -t "${IMAGE}:${DATE}" .

if [ ! -z "$2" ]; then
	echo "Pushing to ECR..."
	./deploy/docker_push.sh "${IMAGE}:${HASH}"
fi
