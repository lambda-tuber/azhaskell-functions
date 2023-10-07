#!/bin/bash

echo ""
echo "[INFO] env"
env

echo ""
echo "[INFO] haskell versions"
ghc --version
cabal --version

echo ""


cd custom-handler
cabal update
cabal clean
cabal build
cabal install --overwrite-policy=always --installdir=. --install-method=copy  --enable-executable-stripping --enable-executable-static


