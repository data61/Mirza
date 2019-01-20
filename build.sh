#!/bin/bash

docker build -f Mirza.Dockerfile --target BUILD .
docker build -f Mirza.Dockerfile --target PKG-SCS -t mirza-scs:latest .
docker build -f Mirza.Dockerfile --target PKG-BR -t mirza-br:latest .
