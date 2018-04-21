#!/usr/bin/env bash

# Ubuntu version
sudo apt-get update && apt-get install haskell-platform

# Build project
cabal build