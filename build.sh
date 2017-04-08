#! /usr/bin/env bash

STACK_IMAGE=${1:-7.8.4}

docker run \
  -v `pwd`:/home/gusdev/aeson-streams \
  -t images.reesd.com/reesd/stack:$STACK_IMAGE \
  cabal install aeson-streams/aeson-streams.cabal
