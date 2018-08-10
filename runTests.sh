#! /usr/bin/env sh

stack test --coverage --fast --ta "$@" -j1
