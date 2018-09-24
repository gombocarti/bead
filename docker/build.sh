#!/bin/bash

cp ../{stack.yaml,Bead.cabal,Dockerfile} ./

# Build image
sudo docker build -t="poora/bead-dev" .
