#!/bin/sh

set -e

./nix/bootstrap.sh

(cd ./nix/ && cabal2nix ./.. > ./pkg.nix)

NIXPKGS_ALLOW_BROKEN=1 nix-build ./nix/docker.nix \
  --option sandbox false \
  -v --show-trace
