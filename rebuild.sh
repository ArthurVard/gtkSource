#!/usr/bin/env zsh

print -- "Compiling and building"
ghc --make site.hs
cabal install && \
./site  clean && \
./site  build && \
./site  watch
