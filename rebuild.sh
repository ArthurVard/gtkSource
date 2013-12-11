#!/usr/bin/env zsh

print -- "Compiling and building"
cabal install && \
./site clean && \
./site build
